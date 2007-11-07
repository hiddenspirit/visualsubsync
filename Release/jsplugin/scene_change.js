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
    
    // Check for scene change around start time

    // Get the scene change previous to the start time
    var scTiming1 = SceneChange.GetPrevious(CurrentSub.Start);
    var scEnd1 = scTiming1 + SceneChange.StopOffset;
    if (scTiming1 != -1 && scEnd1 > CurrentSub.Start) {
      return '1 - Overlap on start: ' + (scEnd1 - CurrentSub.Start) + 'ms';
    }
	  
	  // Get the scene change next to the start time
    var scTiming2 = SceneChange.GetNext(CurrentSub.Start);
    var scStart2 = scTiming2 - SceneChange.StartOffset;
    if (scTiming2 != -1 && scTiming2 < CurrentSub.Stop && scStart2 <= CurrentSub.Start) {
      var scEnd2 = scTiming2 + SceneChange.StopOffset;
      return '2 - Overlap on start: ' + (scEnd2 - CurrentSub.Start) + 'ms';
    }
	  
    // Check for scene change around stop time	  
	  
	  // Get the scene change next to the stop time
    var scTiming3 = SceneChange.GetNext(CurrentSub.Stop);
    var scStart3 = scTiming3 - SceneChange.StartOffset;
    if (scTiming3 != -1 && scStart3 < CurrentSub.Stop) {
      return '3 - Overlap on stop: ' + (CurrentSub.Stop - scStart3) + 'ms';
    }

	  // Get the scene change previous to the stop time
    var scTiming4 = SceneChange.GetPrevious(CurrentSub.Stop);
    var scEnd4 = scTiming4 + SceneChange.StopOffset;
    if (scTiming4 != -1 && scTiming4 > CurrentSub.Start && scEnd4 >= CurrentSub.Stop) {
      var scStart4 = scTiming4 - SceneChange.StartOffset;
      return '4 - Overlap on stop: ' + (CurrentSub.Stop - scStart4) + 'ms';
    }
	
    return '';
  },
  
  FixError : function(CurrentSub, PreviousSub, NextSub) {
    var subtitleContainsSceneChange = SceneChange.Contains(
      CurrentSub.Start - SceneChange.StartOffset,
      CurrentSub.Stop + SceneChange.StopOffset);
    if (!subtitleContainsSceneChange) {
      return;
    }
    
    var Message = "";
    
    // Check if a scene change is overlapping on start and stop at the same time
    var scTimingA = SceneChange.GetNext(CurrentSub.Start);
    var scStartA = scTimingA - SceneChange.StartOffset;
    var scEndA = scTimingA + SceneChange.StopOffset;
    if (scStartA < CurrentSub.Start && scEndA > CurrentSub.Stop) {
      // Don't fix this automatically
      return;
    }
    var scTimingB = SceneChange.GetPrevious(CurrentSub.Stop);
    var scStartB = scTimingB - SceneChange.StartOffset;
    var scEndB = scTimingB + SceneChange.StopOffset;
    if (scStartB < CurrentSub.Start && scEndB > CurrentSub.Stop) {
      // Don't fix this automatically
      return;
    }
    
    // Check for scene change around start time
    
    // Get the scene change previous to the start time
    var scTiming1 = SceneChange.GetPrevious(CurrentSub.Start);
    var scEnd1 = scTiming1 + SceneChange.StopOffset;
    if (scTiming1 != -1 && scEnd1 > CurrentSub.Start) {
      var diff = scEnd1 - CurrentSub.Start;
      CurrentSub.Start = scTiming1 + SceneChange.StopOffset + 1;
      Message += "1";
    }
    
	  // Get the scene change next to the start time
    var scTiming2 = SceneChange.GetNext(CurrentSub.Start);
    var scStart2 = scTiming2 - SceneChange.StartOffset;
    if (scTiming2 != -1 && scTiming2 < CurrentSub.Stop && scStart2 <= CurrentSub.Start) {
      CurrentSub.Start = scTiming2 + SceneChange.StopOffset + 1;
      Message += "2";
    }    

    // Check for scene change around stop time	  
	  
	  // Get the scene change next to the stop time
    var scTiming3 = SceneChange.GetNext(CurrentSub.Stop);
    var scStart3 = scTiming3 - SceneChange.StartOffset;
    if (scTiming3 != -1 && scStart3 < CurrentSub.Stop) {
      CurrentSub.Stop = scTiming3 - SceneChange.StartOffset - 1;
      Message += "3";
    }
    
	  // Get the scene change previous to the stop time
    var scTiming4 = SceneChange.GetPrevious(CurrentSub.Stop);
    var scEnd4 = scTiming4 + SceneChange.StopOffset;
    if (scTiming4 != -1 && scTiming4 > CurrentSub.Start && scEnd4 >= CurrentSub.Stop) {
      CurrentSub.Stop = scTiming4 - SceneChange.StartOffset - 1;
      Message += "4";
    }

    //ScriptLog(Message);
  }
};