const fs = require('fs');
const path = require('path');

module.exports = class Base {

  static generate(routes) {
    var lines = [
      `**FREE`, '',
      `ctl-opt decEdit('0,') datEdit(*YMD.) main(main);`,
      `ctl-opt debug(*yes) bndDir('ILEASTIC':'NOXDB');`,
      `ctl-opt thread(*CONCURRENT);`, '',
      `/include ./headers/ileastic.rpgle`, `/include ./noxdb/headers/jsonParser.rpgle`, `/copy ./structs.rpgle`, '',
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
        `  dcl-s lResponse pointer;`,
        ``,
        `  response.contentType = 'application/json';`,
        ``,
        `  lResponse = json_NewObject();`,
        ``,
        `  if (validate_${route.operationId}(request));`,
        `    response.status = 200;`,
        `    json_SetStr(lResponse:'message':'Successful request');`,
        `  Else;`,
        `    response.status = 500;`,
        `    json_SetStr(lResponse:'message':'Invalid request');`,
        `  Endif;`,
        ``,
        `  il_responseWrite(response: json_AsText(lResponse));`,
        `  JSON_Close(lResponse);`,
        ``,
        `end-proc;`, ``,
      );
    }
  
    lines.push(`/copy ./validation.rpgle`, `/copy ./into.rpgle`, `/copy ./from.rpgle`);
  
    fs.writeFileSync(path.join(process.outputDir, `webapp.rpgle`), lines.join('\n'));
  }
}