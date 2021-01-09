unit Galaxy.Main;

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, System.Generics.Collections, System.Threading,
  System.ImageList, FMX.ImgList, FMX.Layouts, FMX.Effects, FMX.Filter.Effects, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects;

type
  TSprites = class;

  TGameManager = class;

  TShip = class;

  TSprite = class abstract
  private
    FSize: TSizeF;
    FPosition: TPointF;
    FOwner: TSprites;
    procedure SetPosition(const Value: TPointF);
    procedure SetSize(const Value: TSizeF);
    procedure SetOwner(const Value: TSprites);
    function GetRect: TRectF;
  protected
    FIsDeleted: Boolean;
  public
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure Move; virtual; abstract;
    constructor Create(AOwner: TSprites); virtual;
    property Size: TSizeF read FSize write SetSize;
    property Position: TPointF read FPosition write SetPosition;
    property Owner: TSprites read FOwner write SetOwner;
    property Rect: TRectF read GetRect;
    property IsDeleted: Boolean read FIsDeleted;
  end;

  TStar = class(TSprite)
  private
    FSizeD: Single;
    procedure RndS;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
  end;

  TBullet = class(TSprite)
  private
    FSpeed: Single;
    FRotate: Single;
    FOpacity: Single;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
  end;

  TEngine = class(TSprite)
    FShip: TShip;
    FOffset: TPointF;
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
  end;

  TShip = class(TSprite)
  private
    FRotate: Single;
    FImpulse: Single;
    FImpulseX: Single;
    FOffsetY: Single;
    FShootCD: Single;
    FEngineLeft: TEngine;
    FEngineRight: TEngine;
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap); virtual;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

  TSprites = class(TList<TSprite>)
  private
    FManager: TGameManager;
    procedure SetManager(const Value: TGameManager);
  public
    property Manager: TGameManager read FManager write SetManager;
    procedure CheckDeleted;
  end;

  TPlayer = class(TShip)
    procedure Move; override;
  end;

  TEnimy = class(TShip)
  public
    procedure Move; override;
  end;

  TGameManager = class(TComponent)
  private
    FFieldSize: TSizeF;
    FIsRun: Boolean;
    FSprites: TSprites;
    FPlayer: TPlayer;
    FGlobalSpeed: Single;
    FDeltaTime: Single;
    FTicksLastFrame: Single;
    FImages: TImageList;
    procedure SetFieldSize(const Value: TSizeF);
    procedure SetIsRun(const Value: Boolean);
    procedure SetGlobalSpeed(const Value: Single);
    procedure SetImages(const Value: TImageList);
  public
    procedure Run;
    procedure Stop;
    procedure SpawnEnimy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FieldSize: TSizeF read FFieldSize write SetFieldSize;
    property IsRun: Boolean read FIsRun write SetIsRun;
    property Sprites: TSprites read FSprites;
    function IsPressed(Key: Word): Boolean;
    property DeltaTime: Single read FDeltaTime;
    property GlobalSpeed: Single read FGlobalSpeed write SetGlobalSpeed;
    property Images: TImageList read FImages write SetImages;
  end;

  TForm14 = class(TForm)
    Timer1: TTimer;
    GradientAnimation1: TGradientAnimation;
    ImageList1: TImageList;
    LabelFPS: TLabel;
    TimerFPS: TTimer;
    PaintBox: TPaintBox;
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerFPSTimer(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
  private
    FGameManager: TGameManager;
    FPS: Integer;
  public
    property GameManager: TGameManager read FGameManager;
  end;

var
  Form14: TForm14;

implementation

uses
  System.Math, System.Math.Vectors;

{$R *.fmx}

procedure TForm14.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GameManager.Stop;
end;

procedure TForm14.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FGameManager := TGameManager.Create(Self);
  FGameManager.Images := ImageList1;
  FGameManager.FieldSize := ClientRect.Size;

  for i := 0 to 1000 do
    FGameManager.Sprites.Add(TStar.Create(FGameManager.Sprites));

  FGameManager.FPlayer := TPlayer.Create(FGameManager.Sprites);
  FGameManager.FPlayer.Bitmap := ImageList1.Bitmap(TSizeF.Create(300, 300), 0);
  FGameManager.Sprites.Add(FGameManager.FPlayer);

  FGameManager.SpawnEnimy;

  FGameManager.Run;
end;

procedure TForm14.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
//
end;

procedure TForm14.FormResize(Sender: TObject);
begin
  FGameManager.FieldSize := ClientRect.Size;
end;

procedure TForm14.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  Sprite: TSprite;
begin
  Inc(FPS);
  with Canvas do
  begin
    BeginScene;
    for Sprite in GameManager.Sprites do
      Sprite.Draw(Canvas);
    EndScene;
  end;
  Caption := FGameManager.Sprites.Count.ToString;
end;

procedure TForm14.Timer1Timer(Sender: TObject);
begin
  PaintBox.Repaint;
end;

procedure TForm14.TimerFPSTimer(Sender: TObject);
begin
  LabelFPS.Text := 'FPS: ' + FPS.ToString + ' Delta: ' + FGameManager.DeltaTime.ToString;
  FPS := 0;
end;

{ TSprite }

constructor TSprite.Create(AOwner: TSprites);
begin
  inherited Create;
  FIsDeleted := False;
  FOwner := AOwner;
end;

function TSprite.GetRect: TRectF;
begin
  Result := TRectF.Create(FPosition, FSize.Width, FSize.Height);
end;

procedure TSprite.SetOwner(const Value: TSprites);
begin
  FOwner := Value;
end;

procedure TSprite.SetPosition(const Value: TPointF);
begin
  FPosition := Value;
end;

procedure TSprite.SetSize(const Value: TSizeF);
begin
  FSize := Value;
end;

{ TSprites }

procedure TSprites.CheckDeleted;
var
  i: Integer;
begin
  i := 0;
  while (i < Pred(Count)) and (Count > 0) do
    if Items[i].IsDeleted then
    begin
      Items[i].Free;
      Delete(i);
    end
    else
      Inc(i);
end;

procedure TSprites.SetManager(const Value: TGameManager);
begin
  FManager := Value;
end;

{ TGameManager }

procedure TGameManager.SpawnEnimy;
var
  Enimy: TEnimy;
begin
  Enimy := TEnimy.Create(Sprites);
  Enimy.Bitmap := Images.Bitmap(TSizeF.Create(300, 300), 0);
  Sprites.Add(Enimy);
end;

constructor TGameManager.Create(AOwner: TComponent);
begin
  inherited;
  FGlobalSpeed := 4;
  FSprites := TSprites.Create;
  FSprites.Manager := Self;
end;

destructor TGameManager.Destroy;
begin
  Stop;
  FSprites.Free;
  inherited;
end;

function TGameManager.IsPressed(Key: Word): Boolean;
begin
  Result := GetAsyncKeyState(Key) < 0;
end;

procedure TGameManager.Run;
begin
  if FIsRun then
    Exit;
  FIsRun := True;
  TThread.CreateAnonymousThread(
    procedure
    var
      Sprite: TSprite;
      TS: Cardinal;
    begin
      FDeltaTime := 1;
      FTicksLastFrame := TThread.GetTickCount;
      while FIsRun do
      begin
        TS := TThread.GetTickCount;
        Sprites.CheckDeleted;

        for Sprite in Sprites do
          Sprite.Move;

        Sleep(1);
        TThread.Synchronize(nil, Form14.Invalidate);
        FDeltaTime := (TS - FTicksLastFrame) / 1000.0;
        FTicksLastFrame := TS;
      end;
    end).Start;
end;

procedure TGameManager.SetFieldSize(const Value: TSizeF);
begin
  FFieldSize := Value;
end;

procedure TGameManager.SetGlobalSpeed(const Value: Single);
begin
  FGlobalSpeed := Value;
end;

procedure TGameManager.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure TGameManager.SetIsRun(const Value: Boolean);
begin
  FIsRun := Value;
end;

procedure TGameManager.Stop;
begin
  FIsRun := False;
end;

{ TPlayer }

constructor TShip.Create(AOwner: TSprites);
begin
  inherited;
  FEngineLeft := TEngine.Create(AOwner);
  FEngineLeft.FShip := Self;
  FEngineRight := TEngine.Create(AOwner);
  FEngineRight.FShip := Self;
  FRotate := 0;
  FImpulse := 0;
  FImpulseX := 0;
  FShootCD := 0;
  FOffsetY := 0;
  FPosition := TPointF.Create(Owner.Manager.FieldSize.Width / 2, Owner.Manager.FieldSize.Height / 2);
  FSize := TSizeF.Create(90, 100);

  FEngineLeft.FOffset := TPointF.Create(0 + 22 - 1, Size.Height - 6);
  FEngineRight.FOffset := TPointF.Create(Size.Width - 22 + 1, Size.Height - 6);
end;

destructor TShip.Destroy;
begin
  FEngineLeft.Free;
  FEngineRight.Free;
  inherited;
end;

procedure TShip.Draw(Canvas: TCanvas);
begin
  Canvas.DrawBitmap(FBitmap,
    TRectF.Create(0, 0, FBitmap.Width, FBitmap.Height),
    TRectF.Create(FPosition, FSize.Width, FSize.Height),
    1);
  FEngineRight.Draw(Canvas);
  FEngineLeft.Draw(Canvas);
end;

function DivSize(Value: TSizeF; Val: Single): TSizeF;
begin
  Result.Width := Value.Width / Val;
  Result.Height := Value.Height / Val;
end;

function OffsetPoint(Value: TPointF; Angel: Single; Len: Double): TPointF;
begin
  Result.X := Value.X + (Sin(Angel / 180 * pi) * Len);
  Result.Y := Value.Y - (Cos(Angel / 180 * pi) * Len);
end;

procedure TShip.Move;
begin
  FEngineRight.Move;
  FEngineLeft.Move;
end;

procedure TShip.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
end;

{ TPlayer }

procedure TPlayer.Move;
var
  Bullet: TBullet;
begin
  if FImpulse > 0.1 then
    FImpulse := FImpulse - 0.1
  else if FImpulse < -0.1 then
    FImpulse := FImpulse + 0.1
  else
    FImpulse := 0;

  if FImpulseX > 0.1 then
    FImpulseX := FImpulseX - 0.1
  else if FImpulseX < -0.1 then
    FImpulseX := FImpulseX + 0.1
  else
    FImpulseX := 0;

  if Owner.Manager.IsPressed(vkLeft) then
    FImpulseX := Min(20, FImpulseX - 0.5);

  if Owner.Manager.IsPressed(vkRight) then
    FImpulseX := Min(20, FImpulseX + 0.5);

  if Owner.Manager.IsPressed(vkUp) then
    FImpulse := Min(20, FImpulse - 0.5);

  if Owner.Manager.IsPressed(vkDown) then
    FImpulse := Max(-10, FImpulse + 0.5);

  FPosition.Offset(FImpulseX, FImpulse);

  FPosition.Y := Max(Min(FPosition.Y, Owner.Manager.FieldSize.Height - (Size.Height + 20)), 0);
  if (FPosition.Y = Owner.Manager.FieldSize.Height - (Size.Height + 20)) or
    (FPosition.Y = 0)
    then
    FImpulse := 0;

  FPosition.X := Max(Min(FPosition.X, Owner.Manager.FieldSize.Width - (Size.Width)), 0);
  if (FPosition.X = Owner.Manager.FieldSize.Width - (Size.Width)) or
    (FPosition.X = 0)
    then
    FImpulseX := 0;

  if FShootCD > 0 then
    FShootCD := FShootCD - 1;
  if Owner.Manager.IsPressed(vkSpace) and (FShootCD <= 0) then
  begin
    FShootCD := 10;
    Bullet := TBullet.Create(Owner.Manager.Sprites);
    Bullet.Position := GetRect.CenterPoint - DivSize(Bullet.Size, 2);
    Bullet.Position.Offset(0, -Size.Height / 2);
    Bullet.FRotate := FRotate;
    Owner.Manager.Sprites.Add(Bullet);
  end;
  inherited;
end;

{ TStar }

constructor TStar.Create(AOwner: TSprites);
begin
  inherited;
  FPosition := TPointF.Create(Random(Round(Owner.Manager.FieldSize.Width)), Random(Round(Owner.Manager.FieldSize.Height)));
  RndS;
end;

procedure TStar.RndS;
begin
  FSizeD := RandomRange(10, 30) / 10;
  FSize := TSizeF.Create(FSizeD, FSizeD);
end;

procedure TStar.Draw(Canvas: TCanvas);
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.FillEllipse(GetRect, 1);
end;

procedure TStar.Move;

  function OffsetPoint(Value: TPointF; Angel: Single; Len: Double): TPointF;
  begin
    Result.X := Value.X + (Sin(Angel / 180 * pi) * Len);
    Result.Y := Value.Y + (Cos(Angel / 180 * pi) * Len);
  end;

begin

  FPosition.Y := FPosition.Y + FSizeD * Owner.Manager.GlobalSpeed;

  if FPosition.Y + FSize.Height > Owner.Manager.FieldSize.Height then
  begin
    RndS;
    FPosition.Y := -(FSize.Height + Random(100));
    FPosition.X := Random(Round(Owner.Manager.FieldSize.Width));
  end
  else if FPosition.X + FSize.Width > Owner.Manager.FieldSize.Width then
  begin
    RndS;
    FPosition.X := -(FSize.Width + Random(100));
    FPosition.Y := Random(Round(Owner.Manager.FieldSize.Height));
  end
  else if FPosition.Y - FSize.Height < 0 then
  begin
    RndS;
    FPosition.Y := Owner.Manager.FieldSize.Height + (FSize.Height + Random(100));
    FPosition.X := Random(Round(Owner.Manager.FieldSize.Width));
  end
  else if FPosition.X - FSize.Width < 0 then
  begin
    RndS;
    FPosition.X := Owner.Manager.FieldSize.Width + (FSize.Width + Random(100));
    FPosition.Y := Random(Round(Owner.Manager.FieldSize.Height));
  end;
   {if (FPosition.X + FSize.Width < 0) or (FPosition.X + FSize.Width > Owner.Manager.FieldSize.Width) then
    FIsDeleted := True; }
end;

{ TBullet }

constructor TBullet.Create(AOwner: TSprites);
begin
  inherited;
  FSpeed := 10;
  FOpacity := 0;
  FPosition := TPointF.Create(0, 0);
  FSize := TSizeF.Create(9, 30);
end;

procedure TBullet.Draw(Canvas: TCanvas);
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.Lime;
  Canvas.FillRect(GetRect, 3, 3, [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight], FOpacity);
end;

procedure TBullet.Move;
begin
  //FPosition.Y := FPosition.Y - FSpeed;
  FPosition := OffsetPoint(FPosition, FRotate, FSpeed);
  if FOpacity < 0.8 then
    FOpacity := FOpacity + 0.1;
  if FPosition.Y + FSize.Height < 0 then
    FIsDeleted := True;
  if (FPosition.X + FSize.Width < 0) or (FPosition.X + FSize.Width > Owner.Manager.FieldSize.Width) then
    FIsDeleted := True;
end;

{ TEngine }

procedure TEngine.Draw(Canvas: TCanvas);
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.Cyan;
  Canvas.FillEllipse(GetRect, 0.8);
end;

procedure TEngine.Move;
begin
  FSize := TSizeF.Create(5, Max(0, -FShip.FImpulse) + Abs(FShip.FImpulseX) + Random(5));
  Position := TPointF.Create(FShip.Position.X + FOffset.X - Size.Width / 2, FShip.Position.Y + FOffset.Y + 0);
end;

{ TEnimy }

procedure TEnimy.Move;
begin
  inherited;
end;

end.

