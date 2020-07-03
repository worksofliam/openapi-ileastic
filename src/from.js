const fs = require('fs');
const path = require('path');
var lines = [`**FREE`, ``];

module.exports = class From {
  static write() {
    fs.writeFileSync(path.join(process.outputDir, "from.rpgle"), lines.join('\n'));
  }

  static generateProcedure(object, structName) {
    lines.push(`Dcl-Proc from_${structName};`);
    lines.push(`  Dcl-Pi *N Pointer;`, `    ${structName} LikeDS(${structName}_t);`, `  End-Pi;`, ``);
    lines.push(`  Dcl-S lIndex Int(5);`);
    lines.push(`  Dcl-S lArray Pointer;`);
    lines.push(`  Dcl-S lObject Pointer;`);
    lines.push(`  Dcl-S lDocument Pointer;`, '');
    lines.push(`  lDocument = JSON_NewObject();`);
  
    this.generateContents(object, structName, 'lDocument');
  
    lines.push(``, `  Return lDocument;`, `End-Proc;`, ``);
  }

  static generateContents(object, structName, variable) {
    const setTypes = {
      'number': `SetNum`,
      'string': 'SetStr',
      'boolean': 'SetBool',
      'integer': 'SetInt'
    };
  
    var currentProperty;
    for (var name in object.properties) {
      currentProperty = object.properties[name];
  
      switch (currentProperty.type) {
        case 'number':
        case 'string':
        case 'boolean':
        case 'integer':
          lines.push(`  JSON_${setTypes[currentProperty.type]}(${variable}:'${name}':${structName}.${name});`);
          break;
  
        case 'object':
          lines.push('');
          this.generateContents(currentProperty, structName + "." + name);
          break;
  
        case 'array':
  
          if (currentProperty.items.type === "object") {
            lines.push(
              ``,
              `  lArray = JSON_NewArray();`,
              `  For lIndex = 1 to ${structName}.${name}_len;`,
              `    lObject = JSON_NewObject();`
            );

            this.generateContents(currentProperty.items, `${structName}.${name}(lIndex)`, `lObject`);
            lines.push(
              `    JSON_ArrayPush(lArray:lObject);`,
              `  Endfor;`,
              `  JSON_SetPtr(${variable}:'${name}':lArray);`
            );
          } else {
            
            lines.push(
              ``,
              `  For lIndex = 1 to ${structName}.${name}_len;`,
              `    JSON_${setTypes[currentProperty.items.type]}(${variable}:'${name}[]':${structName}.${name}(lIndex));`,
              `  Endfor;`
            );
          }

          break;
      }
    }
  }
}