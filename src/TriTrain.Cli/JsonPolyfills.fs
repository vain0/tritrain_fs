namespace Newtonsoft.Json

  // Trivial implementations just to pass compilation.

  type JsonConvert =
    static member SerializeObject(_) =
      failwith "no impl"

    static member DeserializeXNode(_) =
      failwith "no impl"

namespace Newtonsoft.Json.FSharp
  open Newtonsoft.Json
