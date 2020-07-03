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
    lines.push(`  Dcl-DS lIndex Int(5);`);
    lines.push(`  Dcl-DS lArray Pointer;`);
    lines.push(`  Dcl-DS lDocument Pointer;`);
    lines.push(`  lDocument = JSON_NewObject();`);
  
    this.generateContents(object, structName);
  
    lines.push(``, `  Return lDocument;`, `End-Proc;`, ``);
  }

  static generateContents(object, structName) {
    const setTypes = {
      'number': `SetNum`,
      'string': 'SetStr',
      'boolean': 'SetInd',
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
          lines.push(`  JSON_${setTypes[currentProperty.type]}(lDocument:'${name}':${structName}.${name});`);
          break;
  
        case 'object':
          lines.push('');
          this.generateContents(currentProperty, structName + "." + name);
          break;
  
        case 'array':
          lines.push(
            ``,
            `  For lIndex = 1 to ${structName}.${name}_len;`,
          );
  
          if (currentProperty.items.type === "object") {
            this.generateContents(currentProperty.items, `${structName}.${name}(lIndex)`);
          } else {
            lines.push(`    JSON_${setTypes[currentProperty.items.type]}(${structName}.${name}:'[]':${structName}.${name});`);
          }
  
          lines.push(`  Endfor;`);
          break;
      }
    }
  }
}