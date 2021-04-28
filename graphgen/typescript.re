// get create

// event store

// let add_transaction = {

// };

let create_event = (event) => {
  [%string "
function create%{event.name}(emitter: Address, counter: BigInt, event: %{event.name}): void {
  let event = new %{event.name}(emitter.toHexString().toString() + \"-\" counter.toString())
  event.save()
}
  "]
};