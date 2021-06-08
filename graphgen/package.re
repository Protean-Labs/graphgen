
open Yojson;
module Scripts = {
  [@deriving yojson]
  type t = {
    codegen: string,
    [@key "create-local"] create_local: string,
    build: string,
    [@key "deploy-local"] deploy_local: string,
    deploy: string,
    [@key "deploy-staging"] deploy_staging:string,
    [@key "watch-local"] watch_local:string
  };

  let make = (account:string, project:string) => {
    codegen: "graph codegen --output-dir src/types/",
    create_local: [%string "graph create %{account}/%{project} --node http://127.0.0.1:8020"],
    build: "graph build",
    deploy_local: [%string "graph deploy %{account}/%{project} --debug --ipfs http://localhost:5001 --node http://127.0.0.1:8020"],
    deploy: [%string "graph deploy %{account}/%{project} --ipfs https://api.thegraph.com/ipfs/ --node https://api.thegraph.com/deploy/ --debug"],
    deploy_staging: "graph deploy $THE_GRAPH_GITHUB_USER/$THE_GRAPH_SUBGRAPH_NAME /Protean --ipfs https://api.staging.thegraph.com/ipfs/ --node https://api.staging.thegraph.com/deploy/",
    watch_local: [%string "graph deploy graphprotocol/%{project} --watch --debug --node http://127.0.0.1:8020/ --ipfs http://localhost:5001"]
  };
};

let dev_deps = `Assoc([
  ("@graphprotocol/graph-cli", `String("^0.16.0")),
  ("@graphprotocol/graph-ts", `String("^0.16.0")),
  ("@typescript-eslint/eslint-plugin", `String("^2.0.0")),
  ("@typescript-eslint/parser", `String("^2.0.0")),
  ("eslint", `String("^6.2.2")),
  ("eslint-config-prettier", `String("^6.1.0")),
  ("prettier", `String("^1.18.2")),
  ("typescript", `String("^3.5.2"))
]);

let deps = `Assoc([]);

[@deriving yojson]
type t = {
  name: string,
  version: string,
  license: string,
  scripts: Scripts.t,
  devDependencies: Yojson.Safe.t,
  dependencies: Yojson.Safe.t
};

let make = (subgraph_name:string, account:string, project:string) => {
  name: subgraph_name,
  version: "1.0.0",
  license: "MIT",
  scripts: Scripts.make(account,project),
  devDependencies: dev_deps,
  dependencies: deps
};