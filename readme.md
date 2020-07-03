## OpenAPI validation generator

This is a tool to generate validators, structures and web servers based on OpenAPI and ILEastic/noxDB. It depends on [ILEastic & noxDB](https://github.com/sitemule/ILEastic/) to be installed in order to use the web server and JSON APIs.

## Why?

* Because OpenAPI is awesome
* Spend less time maintaining and coding your API
* Spend more time building your APIs using OpenAPI ([checkout Stoplight](https://stoplight.io/), it is free!)
* Quickly get started building complex APIs using RPGLE
* Generates all code for the web server, validation and structures for all JSON schemas!

### Installation

1. Install [ILEastic & noxDB](https://github.com/sitemule/ILEastic/)
2. Clone this repo & `npm i`
3. Paste your spec into a file called `openapi.yaml`
4. Use `node index` to generate code (see below).

### Generated files

This tool will generate a few files in the `output` directory based on your spec:

* `webapp.rpgle` - the base of the application, which will include the router and procedures to host the APIs. These are example routers that can be changed to suit your needs. The router functions call the validation functions.
* `validation.rpgle` - includes functions for each API (and eventually each model) to validate the headers, query parameters and JSON body if they are applicable. This code is easily re-usable and can easily be brought into existing APIs, even if you don't use the source generated for the base file.
* `structs.rpgle` - structure templates that match your JSON input, output and models
* `intoStructs.rpgle` - generated procedures to take the noxDB JSON structures and store the data into the generated structures (from `structs.rpgle`)
* `outofStructs.rpgle` - generated procedures to take the generated structs (from `structs.rpgle`) and turn them into noxDB JSON stores.
