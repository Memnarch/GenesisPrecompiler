unit GenesisConsts;

interface

type
  TAnsiCharSet =  set of AnsiChar;
  TMessageLevel = (mlNormal, mlSuccess, mlWarning, mlError);
  TAccessLevel = (alPrivate, alProtected, alPublic);
const
  GenesisPrefix = 'Genesis';
  COverridePrefix = 'zzOverriden';
  CWordDelimiters: TAnsiCharSet = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0', '_'];
  CAlphaChars: TAnsiCharSet = ['a'..'z','A'..'Z'];
  CNumericChars: TAnsiCharSet = ['1'..'9','0'];
  CUnderScore: TAnsiCharSet = ['_'];
  CExtension = '.cxx';
implementation

end.
