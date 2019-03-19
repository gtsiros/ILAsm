Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Reflection
Imports System.Reflection.Emit

Public Module IlAsmTest

    Private Const cr As Char = Microsoft.VisualBasic.Chr(13)
    Private Const lf As Char = Microsoft.VisualBasic.Chr(10)

    Public Sub Main()


        Dim a As New TIlAsm.TILAsm(IO.File.ReadAllText("source.ilasm"))

        If a.messages.Count > 0 Then Debug.WriteLine(String.Join(cr & lf, a.messages))

        If a.method IsNot Nothing Then
            Try
                DirectCast(a.method, Action)()
            Catch ex As Exception
                Debug.WriteLine(ex.Message)
            End Try
        End If
        Console.ReadLine()
    End Sub
End Module
