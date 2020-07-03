const yaml = require('js-yaml');
const fs = require('fs');

const Route = require('./route');

const openapiDocument = process.argv[2] || "openapi.yaml"
const outputFile = process.argv[3] || "output/validation.rpgle";
const baseFile = process.argv[4] || "output/webapp.rpgle";

const structsFile = process.argv[5] || "output/structs.rpgle";
const intoStructsFile = process.argv[6] || "output/intoStructs.rpgle";

var document;

var routes = [];
var structsContent = [`**FREE`, ''];
var intoStructsContent = [`**FREE`, ''];

loadDocument();
processDocument();
generateValidator();

generateBase();
generateStructs();
generateIntos();

function loadDocument() {
  try {
    document = yaml.safeLoad(fs.readFileSync(openapiDocument, 'utf8'));
  } catch (e) {
    console.log('Failed to load document!')
    console.log(e);
  }
}

function processDocument() {
  var currentAPI, currentRoute, currentBody;
  console.log(document);

  for (const path in document.paths) {
    for (const requestType in document.paths[path]) {
      currentAPI = document.paths[path][requestType];
      currentRoute = new Route(path, requestType, currentAPI.operationId);

      currentRoute.validator.push(
        `Dcl-Proc validate_${currentRoute.operationId};`,
        `  dcl-pi *n ind;`,
        `    request likeds(IL_REQUEST);`,
        `  end-pi;`,
        ``,
        `  Dcl-S lDocument Pointer;`,
        `  Dcl-S lNode Pointer;`,
        `  Dcl-DS list likeds(JSON_ITERATOR);`,
        ``,
      )

      if (currentAPI.parameters) {
        const queryParms = currentAPI.parameters.filter(x => x.in === "query");
        const headers = currentAPI.parameters.filter(x => x.in === "header");

        for (const header of headers) {
          if (header.required) {
            currentRoute.validator.push(
              `  If (il_getRequestHeader(request:'${header.name}') = *BLANK);`,
              `    Return *Off;`,
              `  Endif;`,
              ``,
            );
          }
        }

        for (const parm of queryParms) {
          if (parm.required) {
            currentRoute.validator.push(
              `  If (il_getParmStr(request:'${parm.name}':'') = *BLANK);`,
              `    Return *Off;`,
              `  Endif;`,
              ``,
            );
          }
        }
      }

      if (currentAPI.requestBody) {
        currentBody = currentAPI.requestBody.content['application/json'].schema;
        generateStruct(currentBody, `${currentRoute.operationId}_requestStruct`);
        getInto(currentBody, `${currentRoute.operationId}_requestStruct`);

        currentRoute.validator.push(
          `  lDocument = JSON_ParseString(request.content.string);`,
          `  If (JSON_Error(lDocument));`,
          `    Return *Off;`,
          `  Else;`,
          ``,
        );

        currentRoute.validator.push(
          `    If (JSON_NodeType(lDocument) <> JSON_OBJECT);`,
          `      Return *Off;`,
          `    Endif;`,
          ``,
        );
        generateAuth(currentBody, 'lDocument');

        currentRoute.validator.push(
          `  Endif;`,
          ``,
        );

      }

      currentRoute.validator.push(
        `  Return *On;`,
        `End-Proc;`, ``
      )
      
      console.log(currentRoute.validator);
      routes.push(currentRoute);
    }
  }

  function generateAuth(object, variable, subobject) {
    console.log(object);
    var currentProperty;
    for (const name in object.properties) {
      currentProperty = object.properties[name];

      switch (currentProperty.type) {
        case 'number':
        case 'string':
        case 'boolean':
        case 'integer':
          if (object.required.includes(name)) {
            currentRoute.validator.push(
              `    If (JSON_NodeType(JSON_Locate(${variable}:'${subobject ? subobject + '.' : ''}${name}')) <> JSON_VALUE);`,
              `      Return *Off;`,
              `    Endif;`,
              ``,
            );
          } else {
            if (currentProperty.default) {
              currentRoute.validator.push(
                `    If (JSON_NodeType(JSON_Locate(${variable}:'${subobject ? subobject + '.' : ''}${name}')) <> JSON_VALUE);`,
                `      lNode = JSON_LocateOrCreate(${variable}:'${subobject ? subobject + '.' : ''}${name}');`,
                `      ${currentProperty.type === "string" ? "json_setStr" : "json_setNum"}(lNode : '' : ${currentProperty.type === "string" ? "'" + currentProperty.default + "'" : currentProperty.default});`,
                `    Endif;`,
                ``,
              )
            }
          }
          break;
        case 'object':
          if (object.required.includes(name)) {
            currentRoute.validator.push(
              `    If (JSON_NodeType(JSON_Locate(${variable}:'${subobject ? subobject + '.' : ''}${name}')) <> JSON_OBJECT);`,
              `      Return *Off;`,
              `    Endif;`,
              ``,
            );

            generateAuth(currentProperty, variable, name);
          }
          break;

        case 'array':
          if (object.required.includes(name)) {
            currentRoute.validator.push(
              `    If (JSON_NodeType(JSON_Locate(${variable}:'${subobject ? subobject + '.' : ''}${name}')) <> JSON_ARRAY);`,
              `      Return *Off;`,
              `    Endif;`,
            );

            if (currentProperty.items.type === "object") {
              currentRoute.validator.push(
                `    list = json_SetIterator(JSON_Locate(${variable}:'${subobject ? subobject + '.' : ''}${name}'));`,
                `    Dow json_ForEach(list);`,
              );

              generateAuth(currentProperty.items, `list.this`);
  
              currentRoute.validator.push(
                `    Enddo;`,
                ``,
              );
            }
          }
          break;
      }
    }
  }
}

function generateValidator() {
  var lines = ['**FREE', ''];

  for (const route of routes) {
    lines.push(...route.validator);
  }

  fs.writeFileSync(`${outputFile}`, lines.join('\n'));
}

function generateBase() {
  var lines = [
    `**FREE`, '',
    `ctl-opt decEdit('0,') datEdit(*YMD.) main(main);`,
    `ctl-opt debug(*yes) bndDir('ILEASTIC':'NOXDB');`,
    `ctl-opt thread(*CONCURRENT);`, '',
    `/include ./headers/ileastic.rpgle`, `/include ./noxdb/headers/jsonParser.rpgle`, '',
    `dcl-proc main;`, ``,
    `  dcl-ds config likeds(il_config);`,
    `  config.port = 8129;`,
    `  config.host = '*ANY';`, ''
  ];

  for (const route of routes) {
    lines.push(`  il_addRoute(config : %paddr(${route.operationId}) : IL_${route.type.toUpperCase()} : '${route.path}');`);
  }

  lines.push(
    ``, `il_listen(config);`,
    `end-proc;`, ``,
  );

  for (const route of routes) {
    lines.push(
      `//***************************************************`, '',
      `dcl-proc ${route.operationId};`,
      `  dcl-pi *n;`,
      `    request  likeds(IL_REQUEST);`,
      `    response likeds(IL_RESPONSE);`,
      `  end-pi;`,
      ``,
      `  dcl-s pJson pointer;`,
      ``,
      `  response.contentType = 'application/json';`,
      ``,
      `  pJson = json_NewObject();`,
      ``,
      `  if (validate_${route.operationId}(request));`,
      `    response.status = 200;`,
      `    json_SetStr(pJson:'message':'Successful request');`,
      `  Else;`,
      `    response.status = 500;`,
      `    json_SetStr(pJson:'message':'Invalid request');`,
      `  Endif;`,

      `  il_responseWrite(response: json_AsText(pJson));`,
      `end-proc;`, ``,
    );
  }

  lines.push(`/copy ./validation.rpgle`)

  fs.writeFileSync(`${baseFile}`, lines.join('\n'));
}

function generateStructs() {
  fs.writeFileSync(`${structsFile}`, structsContent.join('\n'));
}

function generateStruct(object, structName) {
  const types = {
    'number': `Packed(30:15)`,
    'string': 'Pointer',
    'boolean': 'Ind',
    'integer': 'Int(20)'
  };

  var currentStruct = [];

  currentStruct.push(`Dcl-Ds ${structName}_t Qualified Template;`);

  var currentProperty;
  for (const name in object.properties) {
    currentProperty = object.properties[name];

    switch (currentProperty.type) {
      case 'number':
      case 'string':
      case 'boolean':
      case 'integer':
        currentStruct.push(`  ${name} ${types[currentProperty.type]};`);
        break;

      case 'object':
        generateStruct(currentProperty, `${structName}_${name}`);
        currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t);`);
        break;

      case 'array':
        if (currentProperty.items.type === "object") {
          generateStruct(currentProperty.items, `${structName}_${name}`);
          currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t) Dim(100);`);
        } else {
          currentStruct.push(`  ${name} ${types[currentProperty.items.type]} Dim(100);`);
        }
        break;
    }
  }
  
  currentStruct.push(`End-Ds;`, '');

  structsContent.push(...currentStruct);
}

function generateIntos() {
  fs.writeFileSync(intoStructsFile, intoStructsContent.join('\n'));
}

function getInto(object, structName) {
  intoStructsContent.push(`Dcl-Proc into_${structName};`);
  intoStructsContent.push(`  Dcl-Pi *N LikeDS(${structName}_t);`, `    pDocument Pointer;`, `  End-Pi;`, ``);
  intoStructsContent.push(`  Dcl-DS ${structName} LikeDS(${structName}_t);`);
  intoStructsContent.push(`  End-DS;`, ``);

  getIntos(object, structName);

  intoStructsContent.push(``, `  Return ${structName};`, `End-Proc;`, ``);
}

function getIntos(object, structName) {
  const getTypes = {
    'number': `GetNum`,
    'string': 'GetStr',
    'boolean': 'GetInd',
    'integer': 'GetInt'
  };

  var currentProperty;
  for (var name in object.properties) {
    currentProperty = object.properties[name];

    switch (currentProperty.type) {
      case 'number':
      case 'string':
      case 'boolean':
      case 'integer':
        intoStructsContent.push(`  ${structName}.${name} = JSON_${getTypes[currentProperty.type]}(lDocument:'${name}');`);
        break;

      case 'object':
        intoStructsContent.push('');
        getIntos(currentProperty, structName + "." + name);
        break;

      case 'array':
        intoStructsContent.push(
          ``,
          `  list = json_SetIterator(JSON_Locate(lDocument:'${name}'));`,
          `  Dow json_ForEach(list);`,
        );

        if (currentProperty.items.type === "object") {
          getIntos(currentProperty.items, `${structName}.${name}`);
        } else {
          intoStructsContent.push(`    ${structName}.${name}(list.count) = JSON_${getTypes[currentProperty.items.type]}(list.this);`);
        }

        intoStructsContent.push(`  Enddo;`);
        break;
    }
  }
}