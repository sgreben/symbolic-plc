namespace FsPlcCompiler

module Test_data = 
    open FsPlcModel
    open Projects
    open Pous
    open Declarations
    open Values
    open StExpressions
    
    module IL = IlStatements
    
    let VAR_INPUT_X_LINT = 
        { kind = VAR_INPUT
          attr = []
          id = "X"
          typ = Types.BASIC Types.LINT
          ivalue = None }
    
    let VAR_INPUT_S_BOOL = 
        { kind = VAR_INPUT
          attr = []
          id = "S"
          typ = Types.BASIC Types.BOOL
          ivalue = None }
    
    let VAR_INPUT_R_BOOL = 
        { kind = VAR_INPUT
          attr = []
          id = "R"
          typ = Types.BASIC Types.BOOL
          ivalue = None }
    
    let VAR_OUT_C_BOOL = 
        { kind = VAR_INPUT
          attr = []
          id = "C"
          typ = Types.BASIC Types.BOOL
          ivalue = None }
    
    let VAR_INPUT_Y_LINT = 
        { kind = VAR_INPUT
          attr = []
          id = "Y"
          typ = Types.BASIC Types.LINT
          ivalue = None }
    
    let fun_plusone = 
        { name = "PLUS_ONE"
          typ = FUNCTION
          actions = []
          transitions = []
          iface = 
              Some { returnType = Some(Types.BASIC Types.LINT)
                     vars = [ VAR_INPUT_X_LINT ] }
          bodies = [] }
    
    open StExpressions
    open StStatements
    
    let fun_myadd = 
        { name = "MY_ADD"
          typ = FUNCTION
          actions = []
          transitions = []
          iface = 
              Some { returnType = Some(Types.BASIC Types.LINT)
                     vars = [ VAR_INPUT_X_LINT; VAR_INPUT_Y_LINT ] }
          bodies = 
              [ AstST
                    ([ { line = 0L
                         column = 0L }, 
                       AstAssignment
                           (AstDirect "MY_ADD", AstBinop(Add, AstVariable(AstDirect "X"), AstVariable(AstDirect "Y"))) ]) ] }
    
    let fun_mycounter = 
        { name = "MY_COUNTER"
          typ = FUNCTION_BLOCK
          actions = []
          transitions = []
          iface = 
              Some { returnType = None
                     vars = [ VAR_INPUT_X_LINT ] }
          bodies = [] }
    
    let fb_myflipflop = 
        { name = "MY_FLIPFLOP"
          typ = FUNCTION_BLOCK
          actions = []
          transitions = []
          iface = 
              Some { returnType = None
                     vars = [ VAR_INPUT_S_BOOL; VAR_INPUT_R_BOOL ] }
          bodies = [] }
    
    let VAR_MyVar_LINT = 
        { kind = VAR
          attr = []
          id = "MyVar"
          typ = Types.BASIC(Types.LINT)
          ivalue = None }
    
    let VAR_MyCounter_FB = 
        { kind = VAR
          attr = []
          id = "MyCounter"
          typ = Types.TYPE_REFERENCE [ "MY_COUNTER" ]
          ivalue = None }
    
    let VAR_MyFlipFlopFB = 
        { kind = VAR
          attr = []
          id = "MyFlipFlop"
          typ = Types.TYPE_REFERENCE [ "MY_FLIPFLOP" ]
          ivalue = None }
    
    let dummy_pou = 
        { name = "DUMMY_POU"
          typ = PROGRAM
          iface = None
          actions = []
          transitions = []
          bodies = [] }
    
    let tiny_pou_interface = 
        { returnType = None
          vars = [ VAR_MyVar_LINT; VAR_MyCounter_FB; VAR_MyFlipFlopFB ] }
    
    open StExpressions
    open StStatements
    
    let tiny_pou = 
        { dummy_pou with iface = Some tiny_pou_interface
                         bodies = 
                             [ AstST
                                   (([ { line = 0L
                                         column = 0L }, 
                                       AstAssignment
                                           (AstDirect "MyVar", 
                                            AstBinop(Add, AstVariable(AstDirect "MyVar"), AstLiteral(BASIC(INT 10))))
                                       { line = 1L
                                         column = 0L }, 
                                       AstAssignment(AstDirect "MyVar", 
                                                     AstFunction(AstDirect "MY_ADD", 
                                                                 [ AstAnonymous(AstVariable(AstDirect "MyVar"))
                                                                   AstAnonymous(AstLiteral(BASIC(INT 10))) ]))
                                       
                                       { line = 2L
                                         column = 0L }, 
                                       AstIf
                                           (AstCmp(Lt, AstVariable(AstDirect "MyVar"), AstLiteral(BASIC(INT 10))), 
                                            [ { line = 0L
                                                column = 0L }, 
                                              AstAssignment
                                                  (AstDirect "MyVar", 
                                                   AstBinop
                                                       (Add, AstVariable(AstDirect "MyVar"), AstLiteral(BASIC(INT 10)))) ], 
                                            []) ])) ] }
    
    open Tasks
    
    let tiny_resource = 
        { name = "DummyResource"
          tasks = 
              [ { name = "MainTask"
                  properties = 
                      [ INTERVAL(Fixed 1UL)
                        PRIORITY 0 ]
                  pou = [ "MyPou", [ "DUMMY_POU" ] ] } ]
          pouInstances = [ "MyPou", [ "DUMMY_POU" ] ]
          vars = [] }
    
    let tiny_configuration = 
        { name = "DummyConfig"
          vars = []
          resources = [ tiny_resource ] }
    
    let tiny_project = 
        { types = 
              { dataTypes = []
                pous = [ tiny_pou; fun_plusone; fun_myadd; fun_mycounter; fb_myflipflop ] }
          libraries = []
          configurations = [ tiny_configuration ] }

module Tests = 
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open FsCheck
    open FsCheck.NUnit
    open FsPlcModel.Projects
    
    [<TestFixture>]
    type Tests() = 
        [<Test>]
        member x.``Can compile tiny project``() = 
            let compiled = Compile.compile_project Test_data.tiny_project
            printfn "%A" compiled
