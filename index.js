const yaml = require('js-yaml');
const fs = require('fs');
const path = require('path');

const Route = require('./src/route');
const Base = require('./src/base');
const Structs = require('./src/structs');
const Into = require('./src/into');
const From = require('./src/from');

const openapiDocument = "openapi.yaml"
process.outputDir = "output";

var document;
var routes = [];

for (var i = 2; i < process.argv.length; i++) {
  switch (process.argv[i]) {
    case '-i':
      openapiDocument = process.argv[i + 1];
      break;

    case '-o':
      process.outputDir = process.argv[i + 1];
      break;
  }
}

loadDocument();
processDocument();

generateValidator();
Base.generate(routes);
Structs.write();
Into.write();
From.write();

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
        Structs.generateStruct(currentBody, `${currentRoute.operationId}_request`);
        Into.generateProcedure(currentBody, `${currentRoute.operationId}_request`);

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

      if (currentAPI.responses) {
        for (const code in currentAPI.responses) {
          currentBody = currentAPI.responses[code].content['application/json'].schema;
          Structs.generateStruct(currentBody, `${currentRoute.operationId}_${code}_response`);
          From.generateProcedure(currentBody, `${currentRoute.operationId}_${code}_response`)
        }
      }
      
      routes.push(currentRoute);
    }
  }

  function generateAuth(object, variable, subobject) {
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

  fs.writeFileSync(path.join(process.outputDir, `validation.rpgle`), lines.join('\n'));
}