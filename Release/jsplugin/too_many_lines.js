// Too many lines
// (christophe.paris <at> free.fr)
//

var DebugMode = false;

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too many lines',
  Description : 'An error is detected when the number of lines is '+
  	'strictly superior to the specified value.',
  Color : 0x9C00FF, 
  Message : 'Subtitle has too many lines :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMaxLines : { Value : 2, Unit : 'Lines' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
  	var SubText = CurrentSub.StrippedText;
  	var MaxLineLen = 0;
  	var LineArray = SubText.split('\r\n');
  	
  	if(LineArray.length > this.ParamMaxLines.Value)
  	{
  		return '' + LineArray.length + ' lines';
  	} else {
  		return '';
  	}
  }
}