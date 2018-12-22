Imports System
Imports System.Collections.Generic
Imports System.Diagnostics
Imports System.Reflection
Imports System.Reflection.Emit

'' not using this as flags yet, but it made sense at the time
<Flags()>
Public Enum ESeverity
    Information = 1
    Warning = 2
    Failure = 4
    InternalFailure = 8
End Enum

Public Enum EMessage
    empty_source = 0
    invalid_mnemonic
    label_redefinition
    unexpected_argument
    unsupported
    bad_argument
    unused_label
    unmarked_label
    missing_argument
    single_switch_target
    missing_ret
End Enum

Public Class TILAsm

    '' no idea how much of this is thread safe. 
    Private Const cr As Char = Microsoft.VisualBasic.Chr(13)
    Private Const lf As Char = Microsoft.VisualBasic.Chr(10)
    Private Const ht As Char = Microsoft.VisualBasic.Chr(9)
    Private Const publicInstance As BindingFlags = BindingFlags.Public Or BindingFlags.Instance Or BindingFlags.IgnoreCase
    Private Const publicStatic As BindingFlags = BindingFlags.Public Or BindingFlags.Static Or BindingFlags.IgnoreCase
    Private Shared ReadOnly typeCache As TCache(Of String, Type) '' faster!
    Private Shared ReadOnly opcodeCache As TCache(Of String, OpCode)
    Private Shared ReadOnly methodCache As TCache(Of String, MethodInfo)
    Private Shared ReadOnly fieldCache As TCache(Of String, FieldInfo)
    Private Shared ReadOnly ws() As Char = {" "c, ht}
    Private Shared ReadOnly tokenSeparators() As Char = {" "c, ","c, ht}
    'Private Shared ReadOnly stackPop As MethodInfo
    'Private Shared ReadOnly stackPush As MethodInfo

    Private Class TLabel
        Public defined, used As Boolean
        Public lineIndex As Int32
        Public label As Label
        Sub New(label As Label, Optional defined As Boolean = False, Optional used As Boolean = False, Optional lineIndex As Int32 = -1)
            Me.label = label
            Me.defined = defined
            Me.used = used
            Me.lineIndex = lineIndex
        End Sub
    End Class

    Private Class TBadSourceException
        Inherits Exception
        Public lineIndex As Int32
        Public msg As EMessage
        Public severity As ESeverity
        Public Sub New(lineIndex As Int32, message As EMessage, Optional severity As ESeverity = ESeverity.Failure)
            Me.lineIndex = lineIndex
            Me.msg = message
            Me.severity = severity
        End Sub
    End Class

    Public Structure SMessage
        Public lineIndex As Int32
        Public msg As EMessage
        Public severity As ESeverity
        Public Overrides Function ToString() As String
            Return Me.lineIndex.ToString().PadLeft(4) & ": (" & Me.severity.ToString & ") " & Me.msg.ToString.Replace("_"c, " "c)
        End Function
    End Structure

    Shared Sub New()
        typeCache = New TCache(Of String, Type)(AddressOf TypeFinder)
        opcodeCache = New TCache(Of String, OpCode)(AddressOf OpcodeFinder)
        fieldCache = New TCache(Of String, FieldInfo)(AddressOf FieldFinder)
        methodCache = New TCache(Of String, MethodInfo)(AddressOf MethodFinder)
        '' ugly hack for now
        'stackPush = GetType(Stack(Of Object)).GetMethod("Push", New Type() {GetType(Object)})
        'methodCache.Add("Stack.Push Object", stackPush)
        'stackPop = GetType(Stack(Of Object)).GetMethod("Pop")
        'methodCache.Add("Stack.Pop", stackPop)
    End Sub

    Private Shared Function FieldFinder(fieldIdentifier As String) As FieldInfo
        Dim dotIndex As Int32 = fieldIdentifier.IndexOf("."c)
        If dotIndex < 0 Then Return Nothing
        Dim typeName As String = fieldIdentifier.Remove(dotIndex)
        Dim fieldName As String = fieldIdentifier.Substring(dotIndex + 1)
        Dim ty As Type = typeCache(typeName)
        If ty Is Nothing Then Return Nothing
        FieldFinder = ty.GetField(fieldName, publicInstance)
        If FieldFinder Is Nothing Then FieldFinder = ty.GetField(fieldName, publicStatic) '' last attempt
    End Function

    Private Shared Function MethodFinder(methodSpecifier As String) As MethodInfo
        Dim parts() As String = methodSpecifier.Split(ws, 2, StringSplitOptions.RemoveEmptyEntries)
        If parts.Length = 0 Then Return Nothing
        Dim methodIdentifier As String = parts(0)
        Dim dotIndex As Int32 = methodIdentifier.IndexOf("."c)
        If dotIndex < 0 Then Return Nothing
        Dim typeName As String = methodIdentifier.Remove(dotIndex)
        Dim methodName As String = methodIdentifier.Substring(dotIndex + 1)
        Dim ty As Type = typeCache(typeName)
        If ty Is Nothing Then Return Nothing
        Return ty.GetMethod(methodName, If(parts.Length > 1, Array.ConvertAll(parts(1).Split({" "c, ","c}, StringSplitOptions.RemoveEmptyEntries), Function(s) typeCache(s)), Type.EmptyTypes))
    End Function

    Private Shared Function TypeFinder(typeName As String) As Type
        Static assemblies() As Assembly = AppDomain.CurrentDomain.GetAssemblies() '' faster but fixed?
        Dim dotted As String = "." & typeName
        For Each asm As Assembly In assemblies
            For Each t As Type In asm.GetTypes
                If t.FullName.EndsWith(dotted) Then Return t
            Next
        Next
        Return Nothing
    End Function

    Private Shared Function OpcodeFinder(mnemonic As String) As OpCode
        Static OpCodeType As Type = GetType(OpCodes)
        Dim fi As FieldInfo = OpCodeType.GetField(mnemonic.Replace("."c, "_"c), BindingFlags.Public Or BindingFlags.Static Or BindingFlags.IgnoreCase)
        Return If(fi IsNot Nothing, DirectCast(fi.GetValue(Nothing), OpCode), Nothing) ' DirectCast(fi?.GetValue(Nothing), OpCode) '  
    End Function

    'Private Shared Function EmitFinder(ty As Type) As MethodInfo
    '    Static ILGeneratorType As Type = GetType(ILGenerator) '' faster?
    '    Static OpCodeType As Type = GetType(OpCode)
    '    Return ILGeneratorType.GetMethod("Emit", New Type() {OpCodeType, ty})
    'End Function

    '' quick and dirty way to read a string until i get a better parser going
    Private Shared Function Unescape(s As String) As String
        Unescape = ""
        Dim i As Int32 = 0
        Dim f As Boolean = False
        While i < s.Length
            Dim c As Char = s(i)
            If f Then
                f = False
                Select Case c
                    Case "n"c
                        c = lf
                    Case "r"c
                        c = cr
                    Case "t"c
                        c = ht
                    Case "_"c
                        c = " "c
                    Case "\"c
                    Case Else
                        Unescape &= "\"c
                End Select
                Unescape &= c
            Else
                If c = "\"c Then f = True Else Unescape &= c
            End If
            i += 1
        End While
        If f Then Unescape &= "\"c
    End Function

    Public Shared Function OpcDump(o As OpCode) As String
        Return String.Join(", ", ".FlowControl: " & o.FlowControl.ToString, ".Name: " & o.Name, ".OpCodeType: " & o.OpCodeType.ToString, ".OperandType: " & o.OperandType.ToString, ".Size: " & o.Size, ".StackBehaviourPop: " & o.StackBehaviourPop.ToString, ".StackBehaviourPush: " & o.StackBehaviourPush.ToString, ".Value: " & o.Value)
    End Function

    '' instance 

    '' it's up to you to cast it to whatever
    Public ReadOnly method As Object
    Public ReadOnly messages As List(Of SMessage)

    Sub New(source() As String, Optional returnType As Type = Nothing, Optional parameterTypes() As Type = Nothing)
        Dim sw As New Stopwatch
        Me.messages = New List(Of SMessage)
        Dim hasRet As Boolean = False
        Dim lineCount As Int32 = source.Length
        Try
            If lineCount = 0 Then Me.Log(-1, EMessage.empty_source, ESeverity.Failure) '' Throw New TBadSourceException(0, EMessage.empty_source)
            Dim lInfo As TLabel = Nothing
            Dim labels As New Dictionary(Of String, TLabel)
            Dim dm As New DynamicMethod("", returnType, parameterTypes)

            Dim ig As ILGenerator = dm.GetILGenerator

            Dim _Byte As Byte = 0
            Dim _Int16 As Int16 = 0
            Dim _Int32 As Int32 = 0
            Dim _Int64 As Int64 = 0
            Dim _Single As Single = 0F
            Dim _Double As Double = .0
            Dim _Type As Type = Nothing
            Dim _FieldInfo As FieldInfo = Nothing
            Dim _MethodInfo As MethodInfo = Nothing

            sw.Restart()

            For i As Int32 = 0 To lineCount - 1
                Dim line As String = source(i).Trim
                Dim tokens() As String = line.Split(tokenSeparators, StringSplitOptions.RemoveEmptyEntries)
                Dim tokenCount As Int32 = tokens.Length
                If tokenCount = 0 Then Continue For '' blank line

                Dim p As String = tokens(0)

                '' comments
                If p(0) = ";"c Then Continue For

                '' directives? for locals? maybe later
                If p(0) = "."c Then Continue For

                Dim iHasLabel As Int32 = CInt(p(p.Length - 1) <> ":"c) + 1

                If iHasLabel <> 0 Then
                    Dim l As String = p.Remove(p.Length - 1)
                    If labels.TryGetValue(l, lInfo) Then
                        If lInfo.defined Then Me.Log(i, EMessage.label_redefinition, ESeverity.Failure) '' Throw New TBadSourceException(i, EMessage.label_redefinition)
                        lInfo.defined = True
                    Else
                        lInfo = New TLabel(ig.DefineLabel, True)
                        labels.Add(l, lInfo)
                    End If
                    ig.MarkLabel(lInfo.label)
                End If

                If tokens.Length <= iHasLabel Then Continue For '' no opcode

                Dim m As String = tokens(iHasLabel)

                '' don't know how to call generic methods :(
                'If m = "RpnPop" Then
                '    ig.Emit(OpCodes.Callvirt, RpnPop)
                '    Continue For
                'ElseIf m = "RpnPush" Then
                '    ig.Emit(OpCodes.Callvirt, RpnPush)
                '    Continue For
                'End If

                Dim opc As OpCode = opcodeCache(m.ToLower)

                If opc.Size = 0 Then Me.Log(i, EMessage.invalid_mnemonic, ESeverity.Failure)
                Debug.WriteLine(OpcDump(opc))
                Dim argCount As Int32 = tokens.Length - iHasLabel - 1 '' -1 because we know there is an opcode.
                Dim firstArgIndex As Int32 = iHasLabel + 1
                If opc.OperandType = OperandType.InlineNone Then
                    If argCount > 0 Then Me.Log(i, EMessage.unexpected_argument, ESeverity.Warning) '' Me.messages.Add(New SMessage With {.i = i, .m = EMessage.unexpected_argument, .s = ESeverity.Warning})
                    hasRet = hasRet OrElse opc.Value = OpCodes.Ret.Value '' can go from false->true, but not from true->false
                    ig.Emit(opc)
                    Continue For
                End If

                If argCount = 0 Then Me.Log(i, EMessage.missing_argument, ESeverity.Failure) '' doesn't matter what is following, it will certainly need an argument

                If opc.OperandType = OperandType.InlineSwitch Then
                    If argCount = 1 Then Me.Log(i, EMessage.single_switch_target, ESeverity.Warning)
                    Dim switchLabels(argCount - 1) As Label
                    For j As Int32 = 0 To argCount - 1
                        Dim l As String = tokens(j + firstArgIndex)
                        If Not labels.TryGetValue(l, lInfo) Then
                            lInfo = New TLabel(ig.DefineLabel, False, True, i)
                            labels.Add(l, lInfo)
                        End If
                        switchLabels(j) = lInfo.label
                    Next
                    ig.Emit(opc, switchLabels)
                    Continue For
                End If

                'If argCount > 1 Then Me.Log(i, EMessage.unexpected_argument, ESeverity.Warning) '' no other opcode has more than one argument
                'Dim arg As String = args(0).Trim
                If opc.FlowControl = FlowControl.Branch OrElse opc.FlowControl = FlowControl.Cond_Branch Then
                    Dim l As String = tokens(firstArgIndex)
                    If Not labels.TryGetValue(l, lInfo) Then
                        lInfo = New TLabel(ig.DefineLabel, False, True, i)
                        labels.Add(l, lInfo)
                    End If
                    lInfo.used = True
                    ig.Emit(opc, lInfo.label)
                    Continue For
                End If

                Dim arg As String = tokens(firstArgIndex)

                Dim ok As Boolean = False
                Select Case opc.OperandType
                    Case OperandType.ShortInlineI
                        ok = Byte.TryParse(arg, _Byte)
                        If ok Then ig.Emit(opc, _Byte)
                    Case OperandType.InlineI
                        ok = Int32.TryParse(arg, _Int32)
                        If ok Then ig.Emit(opc, _Int32)
                    Case OperandType.InlineI8
                        ok = Int64.TryParse(arg, _Int64)
                        If ok Then ig.Emit(opc, _Int64)
                    Case OperandType.ShortInlineR
                        ok = Single.TryParse(arg, _Single)
                        If ok Then ig.Emit(opc, _Single)
                    Case OperandType.InlineR
                        ok = Double.TryParse(arg, _Double)
                        If ok Then ig.Emit(opc, _Double)
                    Case OperandType.InlineString
                        ok = arg(0) = """"c AndAlso arg(arg.Length - 1) = """"c
                        If ok Then ig.Emit(opc, Unescape(arg.Substring(1, arg.Length - 2)))
                    Case OperandType.InlineMethod
                        Dim j As Int32 = firstArgIndex + 1
                        While j < tokenCount
                            arg &= " "c & tokens(j)
                            j += 1
                        End While
                        _MethodInfo = methodCache(arg)
                        ok = _MethodInfo IsNot Nothing
                        If ok Then ig.Emit(opc, _MethodInfo)
                    Case OperandType.InlineField
                        _FieldInfo = fieldCache(arg)
                        ok = _FieldInfo IsNot Nothing
                        If ok Then ig.Emit(opc, _FieldInfo)
                    Case OperandType.InlineSig
                        Me.Log(i, EMessage.unsupported, ESeverity.InternalFailure)
                    Case OperandType.InlineTok
                        Me.Log(i, EMessage.unsupported, ESeverity.InternalFailure)
                    Case OperandType.InlineType
                        _Type = typeCache(arg)
                        ok = _Type IsNot Nothing
                        If ok Then ig.Emit(opc, _Type)
                    Case OperandType.ShortInlineVar
                        Me.Log(i, EMessage.unsupported, ESeverity.InternalFailure)
                    Case OperandType.InlineVar
                        Me.Log(i, EMessage.unsupported, ESeverity.InternalFailure)
                End Select
                If Not ok Then Me.Log(i, EMessage.bad_argument, ESeverity.Failure)
            Next
            sw.Stop()
            Dim hasUndefined As Boolean = False
            'For Each k As String In labels.Keys
            '    Dim l As TLabel = labels(k)
            For Each l As TLabel In labels.Values
                If Not l.used Then Me.Log(l.lineIndex, EMessage.unused_label, ESeverity.Warning)
                If Not l.defined Then Me.Log(l.lineIndex, EMessage.unmarked_label, ESeverity.Failure, False)
                hasUndefined = hasUndefined Or Not l.defined
            Next
            If Not hasRet Then Me.Log(-1, EMessage.missing_ret, ESeverity.Warning)

            '' create the Type that matches the requested arguments
            '' don't know a better way right now

            Dim typeString As String = "System." ''& If(returnType Is Nothing, "Action", "Func") & If(parameterTypes IsNot Nothing AndAlso parameterTypes.Length > 0, "`" & parameterTypes.Length & "[" & String.Join(",", Array.ConvertAll(parameterTypes, Function(ty) "[" & ty.FullName & "]")) & "]", "")
            Dim tl As New List(Of Type)
            If parameterTypes IsNot Nothing Then tl.AddRange(parameterTypes)
            If returnType IsNot Nothing Then
                tl.Add(returnType)
                typeString &= "Func"
            Else
                typeString &= "Action"
            End If
            If tl.Count > 0 Then typeString &= "`" & tl.Count & "[" & String.Join(",", Array.ConvertAll(tl.ToArray, Function(t) "[" & t.FullName & "]")) & "]"

            If Not hasUndefined Then Me.method = dm.CreateDelegate(Type.GetType(typeString))

        Catch ex As TBadSourceException
            '' ha.
        End Try
        Dim s As Double = sw.ElapsedTicks / Stopwatch.Frequency
#If DEBUG Then
        Debug.WriteLine(s & " s, " & sw.ElapsedTicks & " ticks, " & lineCount / s & " l/s")
#Else
        'Console.WriteLine(s & " s, " & sw.ElapsedTicks & " ticks, " & (lineCount / s) & " l/s")
#End If
    End Sub

    Private Sub Log(i As Int32, m As EMessage, Optional s As ESeverity = ESeverity.Information, Optional throwOnFailure As Boolean = True)
        Me.messages.Add(New SMessage With {.lineIndex = i + 1, .msg = m, .severity = s})
        If s > ESeverity.Warning AndAlso throwOnFailure Then Throw New TBadSourceException(i + 1, m, s)
    End Sub

    '' just for convenience
    Sub New(source As String, Optional returnType As Type = Nothing, Optional parameterTypes() As Type = Nothing)
        Me.New(source.Split({cr, lf}, StringSplitOptions.RemoveEmptyEntries), returnType, parameterTypes)
    End Sub

End Class
