VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too long line',
  Description : 'An error is detected when the line length in a subtitle is strictly superior to the specified value.',
  Color : 0xFFFF37, 
  Message : 'Subtitle has a too long line :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMaxPerLine : { Value : 60, Unit : 'Characters' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
  	var SubText = CurrentSub.Text;
  	var MaxLineLen = 0;
  	var LineArray = SubText.split('\r\n');
  	var LineArrayLen = LineArray.length;
  	var LineLen = 0;
  	for(i=0; i < LineArrayLen; i++)
  	{
  		LineLen = LineArray[i].length;
  		if(LineLen > MaxLineLen)
  			MaxLineLen = LineLen;
  	}
    if (MaxLineLen  > this.ParamMaxPerLine.Value) {
    	return (MaxLineLen + ' ' + this.ParamMaxPerLine.Unit);
    } else {
    	return '';
    }
  }
}
