Imports System
Imports System.Collections.Generic

'' could be implemented with hashing or maybe a btree
'' ... meh
Friend Class TCache(Of K, V)
    Inherits Dictionary(Of K, V)

    Private ReadOnly finder As Func(Of K, V)

    Sub New(finder As Func(Of K, V))
        Me.finder = finder
    End Sub

    Default Public Shadows ReadOnly Property Item(key As K) As V
        Get
            Dim value As V = Nothing
            If Me.TryGetValue(key, value) Then Return value
            value = Me.finder(key)
            Me.Add(key, value)
            Return value
        End Get
    End Property

End Class
