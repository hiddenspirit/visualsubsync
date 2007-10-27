// Scene change plugin by Nathbot
// Modified by Toff for integration in VSS 0.9.11

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Scene change',
  Description : 'Detect and fix subtitle overlapping on a scene change.',
  Color : 0xffe4db, 
  Message : 'Subtitle overlap on a scene change',
  
  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    var subtitleContainsSceneChange = SceneChange.Contains(
      CurrentSub.Start - SceneChange.StartOffset,
      CurrentSub.Stop + SceneChange.StopOffset);
    if (!subtitleContainsSceneChange) {
      return '';
    }

    var scTiming1 = SceneChange.GetPrevious(CurrentSub.Start);
    //CP pertinent ?
    var totalCPStart1 = scTiming1 - SceneChange.StartOffset;
    var totalCPEnd1 = scTiming1 + SceneChange.StopOffset;
    var diff = totalCPEnd1 - CurrentSub.Start;
    if (diff < SceneChange.StopOffSet && diff > 0) {
      return 'Overlap on start: ' + diff + 'ms';
	  }	

    var scTiming2 = SceneChange.GetNext(CurrentSub.Start);
    //CP pertinent ?
    var totalCPStart2 = scTiming2 - SceneChange.StartOffset;
    var totalCPEnd2 = scTiming2 + SceneChange.StopOffset;
    if (totalCPStart2 < CurrentSub.Start) {
      diff = totalCPEnd2 - CurrentSub.Start;
      return 'Overlap on start: ' + diff + 'ms';
    }
    if (totalCPEnd2 > CurrentSub.Stop) {
      diff = CurrentSub.Stop - totalCPStart2;
      return 'Overlap on stop: ' + diff + 'ms';
    }

    var scTiming4 = SceneChange.GetPrevious(CurrentSub.Stop);
    //CP pertinent ?
    var totalCPStart4 = scTiming4 - SceneChange.StartOffset;
    var totalCPEnd4 = scTiming4 + SceneChange.StopOffset;
    if (totalCPStart4 < CurrentSub.Start) {
      diff = totalCPEnd4 - CurrentSub.Start;
      return 'Overlap on start: ' + diff + 'ms';
    }
    if (totalCPEnd4 > CurrentSub.Stop) {
      diff = CurrentSub.Stop - totalCPStart4;
      return 'Overlap on stop: ' + diff + 'ms';
    }
			
    var scTiming3 = SceneChange.GetNext(CurrentSub.Stop);
    //CP pertinent ?
    var totalCPStart3 = scTiming3 - SceneChange.StartOffset;
    var totalCPEnd3 = scTiming3 + SceneChange.StopOffset;
    diff = CurrentSub.Stop - totalCPEnd3;
    if (diff < SceneChange.StartOffSet && diff > 0) {
      return 'Overlap on stop: ' + diff + 'ms';
	  }
	
	  //sinon, CP non pertinent
    return '';
  },
  
  FixError : function(CurrentSub, PreviousSub, NextSub) {
    var scTiming = SceneChange.GetNext(CurrentSub.Start);
    var totalCPStart = scTiming - SceneChange.StartOffset;
    var totalCPEnd = scTiming + SceneChange.StopOffset;
    var Message = "";
    var scTiming1 = SceneChange.GetPrevious(CurrentSub.Start);
    //CP pertinent ?
    var totalCPStart1 = scTiming1 - SceneChange.StartOffset;
    var totalCPEnd1 = scTiming1 + SceneChange.StopOffset;
    var diff = totalCPEnd1 - CurrentSub.Start;
    if (diff < SceneChange.StopOffSet && diff > 0) {
      CurrentSub.Start = scTiming1 + SceneChange.StopOffset + 1;
      Message += "1";
    }	

    var scTiming2 = SceneChange.GetNext(CurrentSub.Start);
    //CP pertinent ?
    var totalCPStart2 = scTiming2 - SceneChange.StartOffset;
    var totalCPEnd2 = scTiming2 + SceneChange.StopOffset;
    //overlap au début ou à la fin ?
    if (totalCPStart2 < CurrentSub.Start){
      CurrentSub.Start = scTiming2 + SceneChange.StopOffset + 1;
      Message += "2";
    }
    if(totalCPEnd2 > CurrentSub.Stop && (totalCPStart2 - CurrentSub.Stop) < (SceneChange.StartOffset + SceneChange.StopOffset)) {
      CurrentSub.Stop = scTiming2 - SceneChange.StartOffset - 1;
      Message += "3";
    }

    var scTiming4 = SceneChange.GetPrevious(CurrentSub.Stop);
    var totalCPStart4 = scTiming4 - SceneChange.StartOffset;
    var totalCPEnd4 = scTiming4 + SceneChange.StopOffset;
    if (totalCPStart4 < CurrentSub.Start && (CurrentSub.Start - totalCPEnd4) < (SceneChange.StartOffset + SceneChange.StopOffset)){
      CurrentSub.Start = scTiming4 + SceneChange.StopOffset + 1;
		  Message += "4";
    }
    if (totalCPEnd4 > CurrentSub.Stop) {
      CurrentSub.Stop = scTiming4 - SceneChange.StartOffset - 1;
      Message += "5";
    }

    var scTiming3 = SceneChange.GetNext(CurrentSub.Stop);
    //CP pertinent ?
    var totalCPStart3 = scTiming3 - SceneChange.StartOffset;
    var totalCPEnd3 = scTiming3 + SceneChange.StopOffset;
    diff = CurrentSub.Stop - totalCPEnd3;
    if (diff < SceneChange.StartOffSet && diff > 0) {
      CurrentSub.Stop = scTiming3 - SceneChange.StartOffset - 1;
      Message += "6";
    }
    //ScriptLog(Message)
  }
};