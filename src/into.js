const fs = require('fs');
const path = require('path');
var lines = [`**FREE`, ''];

module.exports = class Into {
  static write() {
    fs.writeFileSync(path.join(process.outputDir, "into.rpgle"), lines.join('\n'));
  }

  static generateProcedure(object, structName) {
    lines.push(`Dcl-Proc into_${structName};`);
    lines.push(`  Dcl-Pi *N LikeDS(${structName}_t);`, `    pDocument Pointer;`, `  End-Pi;`, ``);
    lines.push(`  Dcl-DS ${structName} LikeDS(${structName}_t);`);
    lines.push(`  Dcl-DS list likeds(JSON_ITERATOR);`);
    lines.push(`  Dcl-S lNode Pointer;`)
  
    this.generateContents(object, structName, 'pDocument');
  
    lines.push(``, `  Return ${structName};`, `End-Proc;`, ``);
  }

  static generateContents(object, structName, variable) {
    const getTypes = {
      'number': `GetNum`,
      'string': 'GetValuePtr',
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
          lines.push(`  ${structName}.${name} = JSON_${getTypes[currentProperty.type]}(${variable}:'${name}');`);
          break;
  
        case 'object':
          lines.push('', `  lNode = JSON_Locate(${variable}:'${name}');`);
          this.generateContents(currentProperty, structName + "." + name, `lNode`);
          break;
  
        case 'array':
          lines.push(
            ``,
            `  list = json_SetIterator(JSON_Locate(${variable}:'${name}'));`,
            `  Dow json_ForEach(list);`,
          );
  
          if (currentProperty.items.type === "object") {
            this.generateContents(currentProperty.items, `${structName}.${name}(list.count)`, 'list.this');
          } else {
            lines.push(`    ${structName}.${name}(list.count) = JSON_${getTypes[currentProperty.items.type]}(list.this);`);
          }
  
          lines.push(`  Enddo;`);
          break;
      }
    }
  }
}