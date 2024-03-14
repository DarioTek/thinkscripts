#// This source code is subject to the terms of the Mozilla Public License 2.0
#// © DDMyke
#//@version=5
#indicator(title="Smoothed RSI w/ VWAP & Moving Average", shorttitle="RSIv"
# Converted and mod by Sam4Cok@Samer800 - 10/2022
declare lower;
input showBarcolor = no;      # 'Barcolor'
input showSignalLine = yes;   # 'Signal Line'
input showBackground = yes;
input rsiSourceInput = close; # 'Source'
input showRSI = yes;
input showRSIvwap = yes;
input VwapTimeFrame = {default DAY, WEEK, MONTH};
input rsiLengthInput = 14;
input rsiOverbought = 75;
input rsiOversold = 25;
input rsiSmoothing = 2;       # 'smoothing'
input showMA = yes;
input maTypeInput = {"SMA", "Bollinger Bands", "EMA", "SMMA (RMA)",default "WMA", "VWMA"};
input maLengthInput = 20;
input bbMultInput = 2.0;      # 'BB StdDev'

def na = Double.NaN;
script nz {
    input data  = close;
    input repl  = 0;
    def ret_val = if IsNaN(data) then repl else data;
    plot return = ret_val;
}
# rsiVWAP
script rsiVWAP {
    input src = close;
    input timeFrame = {default DAY, WEEK, MONTH};
    def v = Volume;
    def cap = GetAggregationPeriod();
    def errorInAggregation =
    timeFrame == timeFrame.DAY and cap >= AggregationPeriod.WEEK or
    timeFrame == timeFrame.WEEK and cap >= AggregationPeriod.MONTH;
    Assert(!errorInAggregation, "timeFrame should be not less than current chart aggregation period");
    def yyyyMmDd = GetYYYYMMDD();
    def periodIndx;
    switch (timeFrame) {
    case DAY:
        periodIndx = yyyyMmDd;
    case WEEK:
        periodIndx = Floor((DaysFromDate(First(yyyyMmDd)) + GetDayOfWeek(First(yyyyMmDd))) / 7);
    case MONTH:
        periodIndx = RoundDown(yyyyMmDd / 100, 0);
}
    def isPeriodRolled = CompoundValue(1, periodIndx != periodIndx[1], yes);
    def volumeSum;
    def volumeVwapSum;
    def volumeVwap2Sum;
    if (isPeriodRolled) {
        volumeSum = v;
        volumeVwapSum = v * src;
        volumeVwap2Sum = v * Sqr(src);
    } else {
        volumeSum = CompoundValue(1, volumeSum[1] + v, v);
        volumeVwapSum = CompoundValue(1, volumeVwapSum[1] + v * src, v * src);
        volumeVwap2Sum = CompoundValue(1, volumeVwap2Sum[1] + v * Sqr(src), v * Sqr(src));
    }
    def price = volumeVwapSum / volumeSum;
    plot return = price;
}
#vwma(source, length)
script VWMA {
    input x = close;
    input y = 15;
    def VWMA = SimpleMovingAvg(x * volume, y) / SimpleMovingAvg(volume, y);
    plot result = VWMA;
}
#ma(smoorsi, maLengthInput, maTypeInput) =>
script ma {
    input smoorsi = close;
    input maLengthInput = 34;
    input maTypeInput = "WMA";
    def ma = if maTypeInput == "SMA" then SimpleMovingAvg(smoorsi, maLengthInput) else
         if maTypeInput == "Bollinger Bands" then SimpleMovingAvg(smoorsi, maLengthInput) else
         if maTypeInput == "EMA" then ExpAverage(smoorsi, maLengthInput) else
         if maTypeInput == "SMMA (RMA)" then WildersSmoothing(smoorsi, maLengthInput) else
         if maTypeInput == "WMA" then WMA(smoorsi, maLengthInput) else
         if maTypeInput == "VWMA" then vwma(smoorsi, maLengthInput) else Double.NaN;
    plot return = ma;
}
def nRSI = RSI(Length = rsiLengthInput, Price = rsiSourceInput);
def smoorsi = WildersSmoothing(nRSI, rsiSmoothing);
def rsiMA = ma(smoorsi, maLengthInput, maTypeInput);
def isBB = maTypeInput == maTypeInput."Bollinger Bands";

#//- VWAP
def RSIvwap = rsiVWAP(smoorsi, VwapTimeFrame);

#//----Plot---------------------------
def StrongBull = smoorsi > 55 and smoorsi > rsiMA and smoorsi > RSIvwap;
def Bullish = smoorsi > 55 and smoorsi < rsiMA and smoorsi > RSIvwap;
def StrongBear = smoorsi < 45 and smoorsi < rsiMA and smoorsi < RSIvwap;
def Bearish = smoorsi > 45 and smoorsi < rsiMA and smoorsi < RSIvwap;
def Neutral = smoorsi > 45 or smoorsi < 55;
def colorchange = if StrongBull then 2 else
                  if Bullish then 1 else
                  if Bearish then -1 else
                  if StrongBear then -2 else 0;

plot rsiPlot = if showRSI then smoorsi else na;    # "Smoothed RSI"
rsiPlot.AssignValueColor(if colorchange == 2 then Color.GREEN else
                         if colorchange == 1 then Color.DARK_GREEN else
                         if colorchange == -2 then Color.RED else
                         if colorchange == -1 then Color.DARK_RED else Color.WHITE);
rsiPlot.SetLineWeight(2);
plot vwapPlot = if showRSIvwap then RSIvwap else na;  # "RSI-based VWAP"
vwapPlot.SetDefaultColor(Color.MAGENTA);

plot maPlot = if showMA then rsiMA else na;    # "RSI-based MA"
maPlot.SetDefaultColor(Color.GRAY);

plot bbUpperBand = if isBB then rsiMA + StDev(smoorsi, maLengthInput) * bbMultInput else na;
bbUpperBand.SetDefaultColor(Color.DARK_GRAY);

plot bbLowerBand = if isBB then rsiMA - StDev(smoorsi, maLengthInput) * bbMultInput else na;
bbLowerBand.SetDefaultColor(Color.DARK_GRAY);

AddCloud(if isBB then bbUpperBand else na, bbLowerBand, CreateColor(76, 10, 127));

#//-------- Hlines
plot rsiUpperBand = rsiOverbought;
rsiUpperBand.SetStyle(Curve.FIRM);
rsiUpperBand.SetDefaultColor(Color.DARK_GRAY);

plot midBand = (rsiOverbought + rsiOversold) / 2 ;
midBand.SetStyle(Curve.SHORT_DASH);
midBand.SetDefaultColor(Color.DARK_GRAY);

plot rsiLowerBand = rsiOversold;
rsiLowerBand.SetStyle(Curve.FIRM);
rsiLowerBand.SetDefaultColor(Color.DARK_GRAY);

AddCloud(if showBackground then rsiUpperBand else na, midBand, CreateColor(6, 45, 3));
AddCloud(if showBackground then midBand else na, rsiLowerBand, CreateColor(66, 0, 0));
#--- Signal Line
def CrossUp = smooRSI crosses above RSIvwap;
def CrossDn = smooRSI crosses below RSIvwap;

def SigUp = if CrossUp and rsiMA > RSIvwap then 1 else 0;
def SigDn = if CrossDn and rsiMA < RSIvwap then 1 else 0;

plot Signal = if (SigUp or SigDn) then 75 else na;
signal.SetHiding(!showSignalLine);
Signal.SetPaintingStrategy(PaintingStrategy.LINE_VS_SQUARES);
Signal.SetLineWeight(2);
Signal.AssignValueColor(if SigUp and smooRSI>=rsiMA then CreateColor(1,255,0) else
                        if SigUp and smooRSI<rsiMA then CreateColor(4,181,4) else
                        if SigDn and smooRSI>rsiMA then Color.DARK_RED else
                        if SigDn and smooRSI<=rsiMA then Color.RED else Color.WHITE);
plot cross = if CrossUp or CrossDn then 75 else na;
cross.SetHiding(!showSignalLine);
cross.SetPaintingStrategy(PaintingStrategy.LINE_VS_POINTS);
cross.AssignValueColor( if CrossUp then CreateColor(2,117,2) else CreateColor(117,2,2));
# ---- BarColor
AssignPriceColor(if !showbarcolor then Color.CURRENT else
                 if colorchange == 2 then Color.GREEN else
                 if colorchange == 1 then Color.DARK_GREEN else
                 if colorchange == -2 then Color.RED else
                 if colorchange == -1 then Color.DARK_RED else Color.GRAY);

#----Div-----------
input PivotLookback  = 5; # "Pivot Lookback"
input DivBull = yes;      # "Plot Bullish"
input DivBear = yes;      # "Plot Bearish"

def divSrc = smoorsi;

def h = high;
def l = low;

script FindPivots {
    input dat = close; # default data or study being evaluated
    input HL  = 0;    # default high or low pivot designation, -1 low, +1 high
    input lbL  = 5;    # default Pivot Lookback Left
    input lbR  = 1;    # default Pivot Lookback Right
    ##############
    def _nan;    # used for non-number returns
    def _BN;     # the current barnumber
    def _VStop;  # confirms that the lookforward period continues the pivot trend
    def _V;      # the Value at the actual pivot point
    ##############
    _BN  = BarNumber();
    _nan = Double.NaN;
    _VStop = if !isNaN(dat) and lbr > 0 and lbl > 0 then
                fold a = 1 to lbR + 1 with b=1 while b do
                    if HL > 0 then dat > GetValue(dat,-a) else dat < GetValue(dat,-a) else _nan;
   if (HL > 0) {
        _V = if _BN > lbL and dat == Highest(dat, lbL+1) and _VStop
            then dat else _nan;
    } else {
        _V = if _BN > lbL and dat == Lowest(dat, lbL+1) and _VStop
            then dat else _nan;
    }
    plot result = if !IsNaN(_V) and _VStop then _V else _nan;
}
#valuewhen (Cond, source, lbr, occurrence)
script valuewhen {
  input cond = 0;
  input src = close;
  input lbr = 0;
  input occurrence = 0;
  def n = occurrence + 1;
  def offset = fold j = 0 to 200 with p=1 while p < n + 1
    do p + ( if p == n then j - n else if cond[j]==yes then 1 else 0 );
  plot price = GetValue(src[lbr], offset-1);
}
#_inRange(cond) =>
script _inRange {
    input cond = yes;
    input rangeUpper = 60;
    input rangeLower = 5;
        def bars = if cond then 0 else bars[1] + 1;
        def inrange =  (rangeLower <= bars) and (bars <= rangeUpper);
plot retrun = inRange;
}
def pl = findpivots(divSrc,-1, PivotLookback, PivotLookback);
def ph = findpivots(divSrc, 1, PivotLookback, PivotLookback);

def plFound = if !isNaN(pl) then 1 else 0;
def phFound = if !isNaN(ph) then 1 else 0;

def vlFound = valuewhen(plFound, divSrc, 0, 1);
def vhFound = valuewhen(phFound, divSrc, 0, 1);

def plPrice = valuewhen(plFound, l, 0, 1);
def phPrice = valuewhen(phFound, h, 0, 1);

#// Regular Bullish
def oscHL = divSrc > vlFound and  _inRange(plFound[1],60,5);
def priceLL = l < plPrice;
def bullCond = DivBull and plFound and oscHL and priceLL;

#// Regular Bearish
def oscLH   = divSrc < vhFound and  _inRange(phFound[1],60,5);
def priceHH = h > phPrice;
def bearCond = DivBear and phFound and oscLH and priceHH;

#------ Bubbles
addchartbubble(bullCond, divSrc, "R", color.GREEN, no);
addchartbubble(bearCond, divSrc, "R", Color.RED, yes);

## END CODE
