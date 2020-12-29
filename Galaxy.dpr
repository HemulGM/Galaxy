program Galaxy;

uses
  System.StartUpCopy,
  FMX.Forms,
  Galaxy.Main in 'Galaxy.Main.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
