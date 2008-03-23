// ---------------------------------------------------------------------------

function JSAction_QuickStats() {
  var subCount = VSSCore.GetSubCount();
  ScriptLog('--- Quick stats ----------');
  ScriptLog('Subtitle count : ' + subCount);
  
  if(subCount > 0) {

	  // Table to store results
	  var rsArray = new Array(ReadingSpeedDef.length);
	  for (var i = 0; i < rsArray.length; i++) {
	    rsArray[i] = 0;
	  }
	  
	  // Iterate over all subtitles to collect stats
	  var sub = VSSCore.GetFirst();
	  while (sub != null) {
	    var rs = getReadingSpeed(sub);
	    var rsIdx = getReadingSpeedIndex(rs);
	    rsArray[rsIdx]++;
	    sub = VSSCore.GetNext(sub);
	  }
	  
	  // Display results
	  for (var i = 0; i < rsArray.length; i++) {
	    var rsCount = rsArray[i];
	    var rsCountPercent = (rsCount * 100) / subCount;
	    ScriptLog(ReadingSpeedDef[i].text + ' = ' + decimal1Round(rsCountPercent)
	    	+ '% (' + rsCount + ')');
	  }
	  
	}
  
  ScriptLog('---');
  
  /*
  var subtitleAt0 = VSSCore.GetSubAt(0);
  ScriptLog('at 0 = ' + ((subtitleAt0 != null) ? subtitleAt0.Text : 'null'));
  
  var firstSubtitle = VSSCore.GetFirst();
  ScriptLog('first = ' + ((firstSubtitle != null) ? firstSubtitle.Text : 'null'));

  var nextSubtitle = VSSCore.GetNext(firstSubtitle);
  ScriptLog('next = ' + ((nextSubtitle != null) ? nextSubtitle.Text : 'null'));
  
  var selectedCursor = VSSCore.GetFirstSelected();
  while (selectedCursor != null) {
    ScriptLog('selected = (' + selectedCursor.Index + ') ' + selectedCursor.Text);
    selectedCursor = VSSCore.GetNextSelected(selectedCursor);
  }
  */
}

VSSCore.RegisterJavascriptAction('JSAction_QuickStats', 'Quick stats', 'Ctrl+M');

// ---------------------------------------------------------------------------