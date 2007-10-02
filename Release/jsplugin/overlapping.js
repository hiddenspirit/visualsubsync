// Overlapping & minimum blank
// (christophe.paris <at> free.fr)

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Overlapping & minimum blank',
  Description : 'An error is detected when the subtitle overlap on next subtitle, or when the minimum blank is not respected.',
  Color : 0xFF3737, 
  Message : 'Subtitle overlap on next subtitle :',
  
  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamFixableOverlap : { Value : 100, Unit : 'ms' },
  ParamMinBlank : { Value : 1, Unit : 'ms' , Description : 'The minimum blank time \n between 2 subtitles.'},

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    if (NextSub == null) {
      return '';
    }
    var OverlapInMs = NextSub.Start - CurrentSub.Stop;
    if ((OverlapInMs > 0) && (OverlapInMs >= this.ParamMinBlank.Value)) {
      if (SceneChange.Contains(CurrentSub.Start, CurrentSub.Stop)) {
        return 'subtitle overlap on scene change';
      }
      return '';
    }
    if (OverlapInMs < 0) {
      return (-OverlapInMs) + 'ms overlap';
    }
    return 'blank is only ' + OverlapInMs + 'ms';
  },
  
  FixError : function(CurrentSub, PreviousSub, NextSub) {
    if (NextSub == null) {
      return '';
    }
    
    var OverlapInMs = NextSub.Start - CurrentSub.Stop;
    	
    if (((OverlapInMs > 0) && (OverlapInMs > this.ParamMinBlank.Value)) ||
        (-OverlapInMs > this.ParamFixableOverlap.Value)) {
      return '';
    }
    // Fix the overlap by dividing it by 2
    var MiddlePoint = (CurrentSub.Stop + (OverlapInMs / 2))
    var HalfOffset = (this.ParamMinBlank.Value / 2);
    CurrentSub.Stop = MiddlePoint - HalfOffset;
    NextSub.Start = CurrentSub.Stop + this.ParamMinBlank.Value;
    
    // Special case when OverlapInMs == 0
    if (NextSub.Start == CurrentSub.Stop) {
			NextSub.Start += 1;
    }
  }
}


