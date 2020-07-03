module.exports = class Route {
  constructor(path, type, operationId) {
    this.path = path;
    this.type = type;
    this.operationId = operationId.replace(new RegExp('-', 'g'), '_');

    this.validator = [];
  }
}