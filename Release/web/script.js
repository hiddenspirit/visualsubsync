function OpenDetailsWindow(index)
{
	newwin = window.open("details.shtml?idx="+index,"SubtitleDetails","width=630,height=340,resizable");
	newwin.focus();
}

function Highlighter(word)
{
	var count = 0;
	if(word!=null && word.length > 0)
	{			
	  var all = document.all ? document.all.tags('TD') : document.getElementsByTagName('TD');  
	  for (var e = 0; e < all.length; e++)
	  {
	    if (all[e].className == "CellText" || all[e].className == "CellTextH")
	    {
	    	var TDElement = all[e];    	
				if (TDElement.innerHTML.toLowerCase().indexOf(word) != -1)
				{
					if(TDElement.className == "CellText")
						TDElement.className = "CellTextH";
					count++;
				} else {
					if(TDElement.className == "CellTextH")		
						TDElement.className = "CellText";
				}
	    }
	  }
	}
  return count + " line"+ ((count > 1) ? "s" : "") +" found.";
}

function HighlighterClear()
{
	var all = document.all ? document.all.tags('TD') : document.getElementsByTagName('TD');
  for (var e = 0; e < all.length; e++)
    if (all[e].className == "CellTextH")
      all[e].className = "CellText";
}