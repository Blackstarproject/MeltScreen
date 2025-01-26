Imports System.IO
Imports System.Runtime.InteropServices

Public Class Form1

    'THIS APP WILL STREAM A TEXT FILE AS AN INTRODUCTION
    'I ADDED IT IN THE EVENT YOU WERE CURIOUS HOW THAT WORKED. NOW I'LL SHOW YOU THE CODE
#Region "Melting Screen"

    Private Shared screenWidth As Integer

    Private Shared screenHeight As Integer

    Const SM_CXSCREEN As Integer =
        0
    Const SM_CYSCREEN As Integer =
        1
    Const WS_EX_TOPMOST As UInteger =
        &H8
    Const WS_POPUP As UInteger =
        &H80000000UI
    Const WM_CREATE As UInteger =
        &H1
    Const WM_CLOSE As UInteger =
        &H10
    Const WM_PAINT As UInteger =
        &HF
    Const WM_DESTROY As UInteger =
        &H2
    Const SRCCOPY As UInteger =
        &HCC0020

    Private Shared ReadOnly HWND_DESKTOP As IntPtr =
        IntPtr.Zero

    <StructLayout(LayoutKind.Sequential)>
    Public Structure POINT
        Public X As Integer
        Public Y As Integer
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Public Structure MSG
        Public hwnd As IntPtr
        Public wParam As IntPtr
        Public lParam As IntPtr
        Public message As UInteger
        Public time As UInteger
        Public pt As POINT
    End Structure

    <StructLayout(LayoutKind.Sequential,
                  CharSet:=CharSet.Unicode)>
    Friend Structure WNDCLASS
        Public style As UInteger
        Public lpfnWndProc As IntPtr
        Public cbClsExtra As Integer
        Public cbWndExtra As Integer
        Public hInstance As IntPtr
        Public hIcon As IntPtr
        Public hCursor As IntPtr
        Public hbrBackground As IntPtr
        <MarshalAs(UnmanagedType.LPWStr)>
        Public lpszMenuName As String
        <MarshalAs(UnmanagedType.LPWStr)>
        Public lpszClassName As String

    End Structure

    <DllImport("user32.dll")>
    Public Shared Function GetSystemMetrics(nIndex As Integer) As Integer
    End Function


    <DllImport("user32.dll", SetLastError:=True,
               CharSet:=CharSet.Unicode)>
    Private Shared Function RegisterClassW(
    <[In]> ByRef lpWndClass As WNDCLASS) As UShort
    End Function


    <DllImport("user32.dll", SetLastError:=True,
               CharSet:=CharSet.Unicode)>
    Private Shared Function LoadCursorW(hInstance As IntPtr,
                                        lpCursorName As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll")>
    Private Shared Function GetDC(hWnd As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll")>
    Private Shared Sub PostQuitMessage(nExitCode As Integer)
    End Sub

    <DllImport("user32.dll")>
    Private Shared Function DefWindowProc(hWnd As IntPtr,
                                          Msg As UInteger,
                                          wParam As IntPtr,
                                          lParam As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function CreateWindowExW(
    dwExStyle As UInteger,
    <MarshalAs(UnmanagedType.LPWStr)> lpClassName As String,
    <MarshalAs(UnmanagedType.LPWStr)> lpWindowName As String,
    dwStyle As UInteger,
    x As Integer,
    y As Integer,
    nWidth As Integer,
    nHeight As Integer,
    hWndParent As IntPtr,
    hMenu As IntPtr,
    hInstance As IntPtr,
    lpParam As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function GetMessage(<Out> ByRef lpMsg As MSG,
                                       hWnd As IntPtr,
                                       wMsgFilterMin As UInteger,
                                       wMsgFilterMax As UInteger) As Integer
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function SetTimer(hWnd As IntPtr,
                                     nIDEvent As IntPtr,
                                     uElapse As UInteger,
                                     lpTimerFunc As TimerProc) As IntPtr
    End Function
    Friend Delegate Sub TimerProc(hWnd As IntPtr,
                                  uMsg As UInteger,
                                  nIDEvent As IntPtr,
                                  dwTime As UInteger)

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function TranslateMessage(
    <[In]> ByRef lpMsg As MSG) As Boolean
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function DispatchMessage(
    <[In]> ByRef lpMsg As MSG) As IntPtr
    End Function

    <DllImport("user32.dll")>
    Private Shared Function ReleaseDC(hWnd As IntPtr,
                                      hDC As IntPtr) As Integer
    End Function

    <DllImport("user32.dll")>
    Private Shared Function ValidateRect(hWnd As IntPtr,
                                         lpRect As IntPtr) As Boolean
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function KillTimer(hWnd As IntPtr,
                                      uIDEvent As IntPtr) As Boolean
    End Function

    <DllImport("gdi32.dll", SetLastError:=True)>
    Private Shared Function BitBlt(hdcDest As IntPtr,
    nXDest As Integer,
    nYDest As Integer,
    nWidth As Integer,
    nHeight As Integer,
    hdcSrc As IntPtr,
    nXSrc As Integer,
    nYSrc As Integer,
    dwRop As UInteger) As Boolean
    End Function

    <DllImport("user32.dll")>
    Private Shared Function ShowWindow(hWnd As IntPtr,
                                       nCmdShow As Integer) As Boolean
    End Function

    <DllImport("kernel32.dll")>
    Private Shared Function GetConsoleWindow() As IntPtr
    End Function

    Friend Delegate Function WndProcDelegate(hWnd As IntPtr,
                                             uMsg As UInteger,
                                             wParam As IntPtr,
                                             lParam As IntPtr) As IntPtr

#End Region

    Private ReadOnly fileName As String =
            Environment.GetFolderPath(Environment.SpecialFolder.Desktop)

    Private ReadOnly path As String =
            "\WARNING.txt"

    Private Sub Form1_Load(sender As Object,
                           e As EventArgs) Handles MyBase.Load

        TEXT_CREATOR_HOST()

    End Sub

    Private Sub Screen_Morph()

        Dim hWndConsole As IntPtr =
            GetConsoleWindow()

        screenWidth =
            GetSystemMetrics(SM_CXSCREEN)

        screenHeight =
            GetSystemMetrics(SM_CYSCREEN)

        Dim hInstance =
            Marshal.GetHINSTANCE(GetType(Application).Module)

        Dim wndClass As New WNDCLASS With {
    .style =
    0,
    .lpfnWndProc =
    Marshal.GetFunctionPointerForDelegate(New WndProcDelegate(AddressOf WndProc)),
    .cbClsExtra =
    0,
    .cbWndExtra =
    0,
    .hInstance =
    hInstance,
    .hIcon =
    IntPtr.Zero,
    .hCursor =
    LoadCursorW(IntPtr.Zero,
                           New IntPtr(32512)), '<< IDC_ARROW
    .hbrBackground =
    IntPtr.Zero,
    .lpszMenuName =
    "",
    .lpszClassName =
    "MeltingScreen"
}

        Dim classAtom =
            RegisterClassW(wndClass)

        If classAtom =
            0 Then
            Dim [error] As Integer =
                Marshal.GetLastWin32Error()
            Debug.WriteLine("Error",
                            +[error])
        Else
            Dim hWnd =
                CreateWindowExW(WS_EX_TOPMOST,
                                       wndClass.lpszClassName,
                                       "MyWindow",
                                       WS_POPUP,
                                       0,
                                       0,
                                       screenWidth,
                                       screenHeight,
                                       HWND_DESKTOP,
                                       IntPtr.Zero,
                                       hInstance,
                                       IntPtr.Zero)

            Dim msg As MSG

            While GetMessage(msg,
                             IntPtr.Zero,
                             0,
                             0) <> 0

                TranslateMessage(msg)

                DispatchMessage(msg)

            End While

        End If

    End Sub

    Private Shared Sub TimerCallback(hWnd As IntPtr,
                                     uMsg As UInteger,
                                     nIDEvent As IntPtr,
                                     dwTime As UInteger)
        Dim Wnd =
            GetDC(hWnd)

        Dim random As New Random()

        Dim x As Integer =
            random.[Next](0,
                          screenWidth) - 200 / 2
        Dim y =
            random.[Next](0,
                          15)
        Dim width =
            random.[Next](0,
                          200)
        BitBlt(Wnd,
               x,
               y,
               width,
               screenHeight,
               Wnd,
               x,
               0,
               SRCCOPY)

        ReleaseDC(hWnd, Wnd)

    End Sub

    Private Shared Function WndProc(hWnd As IntPtr,
                                    uMsg As UInteger,
                                    wParam As IntPtr,
                                    lpParam As IntPtr) As IntPtr

        Select Case uMsg

            Case WM_CREATE

                Dim Desktop =
                    GetDC(HWND_DESKTOP)

                Dim Window =
                    GetDC(hWnd)

                BitBlt(Window,
                       0,
                       0,
                       screenWidth,
                       screenHeight,
                       Desktop,
                       0,
                       0,
                       SRCCOPY)

                ReleaseDC(hWnd,
                          Window)

                ReleaseDC(HWND_DESKTOP,
                          Desktop)

                SetTimer(hWnd,
                         IntPtr.Zero,
                         100,
                         New TimerProc(AddressOf TimerCallback))

                ShowWindow(hWnd, 5)

            Case WM_PAINT
                ValidateRect(hWnd, IntPtr.Zero)

            Case WM_DESTROY
                KillTimer(hWnd, IntPtr.Zero)

                PostQuitMessage(0)

            Case WM_CLOSE
                KillTimer(hWnd,
                          IntPtr.Zero)

                PostQuitMessage(0)

            Case Else

                Return DefWindowProc(hWnd,
                                     uMsg,
                                     wParam,
                                     lpParam)

        End Select

        Return IntPtr.Zero

    End Function

    'THIS SENDS A TEXT FILE TO TARGETED HOST AND OPENS ITSELF AFTER
    Private Sub TEXT_CREATOR_HOST()

        Timer1.Start()

        Using writer As New StreamWriter(fileName & path,
                                         True)
            writer.Write(vbCrLf & "WELCOME TO MELTING SCREEN:" &
                         vbNewLine)
            writer.Write("MELTING SCREEN INITIATING..." &
                         vbNewLine)
            writer.WriteLine("TO CLOSE, HIT YOUR WINDOW KEY," & "RIGHT CLICK YOUR APP IN TASKBAR TO CLOSE" &
                             vbNewLine)
            writer.WriteLine(" " &
                             vbNewLine)
            writer.Write("HOPE YOU ENJOYED THE VIDEO" &
                         vbNewLine)
            writer.WriteLine("MORE PROJECTS COMING SOON")

            writer.WriteLine($"{vbNewLine &
                             vbNewLine & Date.Now.ToLongTimeString()} 
                             {Date.Now.ToLongDateString()}")
            writer.Flush()

            writer.Close()

        End Using

    End Sub

    Private Sub Timer1_Tick(sender As Object,
                            e As EventArgs) Handles Timer1.Tick
        Screen_Morph()

    End Sub

    Private Sub Timer2_Tick(sender As Object,
                            e As EventArgs) Handles Timer2.Tick

        'OPEN MESSAGE AFTER IT'S CREATION
        'Desktop Path
        Dim desktopPath =
            My.Computer.FileSystem.SpecialDirectories.Desktop

        ' Concatenate desktop path and file name
        Dim filePath =
            desktopPath &
            "/WARNING.txt"

        Process.Start(filePath)

        Timer2.Stop()

    End Sub

End Class