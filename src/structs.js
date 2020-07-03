const fs = require('fs');
var lines = [`**FREE`, ''];

module.exports = class Structs {
  static write() {
    fs.writeFileSync(`output/structs.rpgle`, lines.join('\n'));
  }

  static generateStruct(object, structName) {
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
          this.generateStruct(currentProperty, `${structName}_${name}`);
          currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t);`);
          break;
  
        case 'array':
          currentStruct.push(`  ${name}_len Uns(5);`);
          if (currentProperty.items.type === "object") {
            this.generateStruct(currentProperty.items, `${structName}_${name}`);
            currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t) Dim(100);`);
          } else {
            currentStruct.push(`  ${name} ${types[currentProperty.items.type]} Dim(100);`);
          }
          break;
      }
    }
    
    currentStruct.push(`End-Ds;`, '');
  
    lines.push(...currentStruct);
  }
}