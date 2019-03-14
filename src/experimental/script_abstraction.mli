type ScriptKey = 
| ScriptPubKey of string
| PubKey of string
| Address of string


type ScriptAbstraction =
| P2PKH of ScriptKey
| P2WPKH of ScriptKey
| P2SH of ScriptKey
| P2WSH of ScriptKey
| P2PK of ScriptKey
| Nulldata of string
| Multisig of int * PubKey list
| AbsoluteTimelock of Script.t * ScriptAbstraction
| RelativeTimelock of Script.t * ScriptAbstraction
| IfElse of ScriptAbstraction * ScriptAbstraction
| Hashlock256 of Script.t * ScriptPubKey
| Hashlock160 of Script.t * ScriptPubKey