unit Galaxy.Main;

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Ani, System.Generics.Collections, System.Threading, System.ImageList,
  FMX.ImgList;

type
  TSprites = class;

  TGameManager = class;

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
    FSpeed: Single;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
  end;

  TBullet = class(TSprite)
  private
    FSpeed: Single;
    FRotate: Single;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
  end;

  TPlayer = class(TSprite)
  private
    FBitmap: TBitmap;
    FRotate: Single;
    procedure SetBitmap(const Value: TBitmap);
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure Move; override;
    constructor Create(AOwner: TSprites); override;
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

  TGameManager = class(TComponent)
  private
    FFieldSize: TSizeF;
    FIsRun: Boolean;
    FSprites: TSprites;
    procedure SetFieldSize(const Value: TSizeF);
    procedure SetIsRun(const Value: Boolean);
  public
    procedure Run;
    procedure Stop;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FieldSize: TSizeF read FFieldSize write SetFieldSize;
    property IsRun: Boolean read FIsRun write SetIsRun;
    property Sprites: TSprites read FSprites;
    function IsPressed(Key: Word): Boolean;
  end;

  TForm14 = class(TForm)
    Timer1: TTimer;
    GradientAnimation1: TGradientAnimation;
    ImageList1: TImageList;
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FGameManager: TGameManager;
    FPlayer: TPlayer;
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
  FGameManager.FieldSize := ClientRect.Size;

  for i := 0 to 1000 do
    FGameManager.Sprites.Add(TStar.Create(FGameManager.Sprites));

  FPlayer := TPlayer.Create(FGameManager.Sprites);
  FPlayer.Bitmap := ImageList1.Bitmap(TSizeF.Create(300, 300), 0);
  FGameManager.Sprites.Add(FPlayer);

  FGameManager.Run;
end;

procedure TForm14.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  Sprite: TSprite;
begin
  with Canvas do
  begin
    BeginScene;
    for Sprite in GameManager.Sprites do
      Sprite.Draw(Canvas);
    EndScene;
  end;
  Caption := FGameManager.Sprites.Count.ToString;
end;

procedure TForm14.FormResize(Sender: TObject);
begin
  FGameManager.FieldSize := ClientRect.Size;
end;

procedure TForm14.Timer1Timer(Sender: TObject);
begin
  Invalidate;
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

constructor TGameManager.Create(AOwner: TComponent);
begin
  inherited;
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
  TTask.Run(
    procedure
    var
      Sprite: TSprite;
    begin
      while FIsRun do
      begin
        Sprites.CheckDeleted;

        for Sprite in Sprites do
          Sprite.Move;

        Sleep(1);
      end;
    end);
end;

procedure TGameManager.SetFieldSize(const Value: TSizeF);
begin
  FFieldSize := Value;
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

constructor TPlayer.Create(AOwner: TSprites);
begin
  inherited;
  FRotate := 0;
  FPosition := TPointF.Create(Owner.Manager.FieldSize.Width / 2, Owner.Manager.FieldSize.Height / 2);
  FSize := TSizeF.Create(90, 100);
end;

procedure TPlayer.Draw(Canvas: TCanvas);
var
  OldMatrix, Matrix: TMatrix;
begin              {
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.Maroon;
  Canvas.FillRect(GetRect, 0, 0, [], 1); }
  OldMatrix := Canvas.Matrix;

  Matrix := TMatrix.CreateTranslation(-FSize.Width / 2, -FSize.Height / 2) * TMatrix.CreateRotation(DegToRad(FRotate));
  Matrix := Matrix * TMatrix.CreateTranslation(FPosition.X, FPosition.Y);
  Canvas.SetMatrix(Matrix);

  Canvas.DrawBitmap(FBitmap,
    TRectF.Create(0, 0, FBitmap.Width, FBitmap.Height),
    TRectF.Create(0, 0, FSize.Width, FSize.Height),
    1);
  Canvas.SetMatrix(OldMatrix);
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

procedure TPlayer.Move;
var
  Bullet: TBullet;
begin
  if Owner.Manager.IsPressed(vkLeft) then
    //FPosition.Offset(-10, 0);
    FRotate := FRotate - 1;
  if Owner.Manager.IsPressed(vkRight) then
    //FPosition.Offset(+10, 0);
    FRotate := FRotate + 1;
  if Owner.Manager.IsPressed(vkUp) then
    FPosition := OffsetPoint(FPosition, FRotate, 3);
  if Owner.Manager.IsPressed(vkDown) then
    FPosition := OffsetPoint(FPosition, FRotate, -3);

  if Owner.Manager.IsPressed(vkSpace) then
  begin
    Bullet := TBullet.Create(Owner.Manager.Sprites);
    Bullet.Position := GetRect.CenterPoint;
    Bullet.FRotate := FRotate;
    Owner.Manager.Sprites.Add(Bullet);
  end;
end;

procedure TPlayer.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
end;

{ TStar }

constructor TStar.Create(AOwner: TSprites);
begin
  inherited;
  FSpeed := RandomRange(50, 100) / 5;
  FPosition := TPointF.Create(Random(Round(Owner.Manager.FieldSize.Width)), Random(Round(Owner.Manager.FieldSize.Height)));
  FSize := TSizeF.Create(RandomRange(2, 5), RandomRange(2, 5));
end;

procedure TStar.Draw(Canvas: TCanvas);
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.FillEllipse(GetRect, 1);
end;

procedure TStar.Move;
begin
  FPosition.Y := FPosition.Y + FSpeed;
  if FPosition.Y + FSize.Height > Owner.Manager.FieldSize.Height then
  begin
    FSize := TSizeF.Create(RandomRange(2, 5), RandomRange(2, 5));
    FPosition.Y := -(FSize.Height + Random(100));
    FPosition.X := Random(Round(Owner.Manager.FieldSize.Width));
  end;
  if (FPosition.X + FSize.Width < 0) or (FPosition.X + FSize.Width > Owner.Manager.FieldSize.Width) then
    FIsDeleted := True;
end;

{ TBullet }

constructor TBullet.Create(AOwner: TSprites);
begin
  inherited;
  FSpeed := 10;
  FPosition := TPointF.Create(0, 0);
  FSize := TSizeF.Create(9, 9);
end;

procedure TBullet.Draw(Canvas: TCanvas);
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColorRec.Lime;
  Canvas.FillEllipse(GetRect, 1);
end;

procedure TBullet.Move;
begin
  //FPosition.Y := FPosition.Y - FSpeed;
  FPosition := OffsetPoint(FPosition, FRotate, FSpeed);

  if FPosition.Y + FSize.Height < 0 then
    FIsDeleted := True;
  if (FPosition.X + FSize.Width < 0) or (FPosition.X + FSize.Width > Owner.Manager.FieldSize.Width) then
    FIsDeleted := True;
end;

end.

