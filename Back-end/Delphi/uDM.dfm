object DM: TDM
  Height = 480
  Width = 640
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=D:\Magno_Vue\magno\Back-end\database\DB_AGENDA.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Server=192.168.10.10'
      'Port=3050'
      'Protocol=TCPIP'
      'CharacterSet=ISO8859_1'
      'DriverID=FB')
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvKeepConnection]
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 64
    Top = 48
  end
  object FDPhysFBDriverLink: TFDPhysFBDriverLink
    VendorLib = 'C:\Windows\SysWOW64\FBCLIENT.DLL'
    Left = 200
    Top = 48
  end
end
