## OpenAPI validation generator

This is a tool to generator a validator based on OpenAPI for ILEastic/noxDB. It depends on [ILEastic & noxDB](https://github.com/sitemule/ILEastic/) to be installed in order to use the web server and JSON APIs.

### Installation

1. Install [ILEastic & noxDB](https://github.com/sitemule/ILEastic/)
2. Clone this repo & `npm i`
3. Paste your spec into a file called `openapi.yaml`
4. Use `node index` to generate code (see below).

### Generated files

This tool will generate two files in the `output` directory based on your spec:

* `webapp.rpgle` - the base of the application, which will include the router and procedures to host the APIs. This are example routers that can be changed to suit your needs. The router functions call the validation functions.
* `validation.rpgle` - includes functions for each API (and eventually each model) to validate the headers, query parameters and JSON body if they are applicable. This code is easily re-usable and can easily be brought into existing APIs, even if you don't use the source generated for the base file.