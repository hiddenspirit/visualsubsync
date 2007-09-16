This directory contains JavaScript plugins for VisualSubSync.

For more information about JavaScript check this links:
http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide
http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference


VSS specific functions and properties:

ScriptLog(Message) : Display a message in the log window
SetStatusBarText(Message) : Display a message in the status bar


Subtitle.Start : Start time in ms of the subtitle
Subtitle.Stop : Stop time in ms of the subtitle
Subtitle.Text : Subtitle's text
Subtitle.StrippedText : Subtitle's text without any tags


SceneChange.StartOffset : The offset in ms to keep before a scene change (value set in the preferences dialog)
SceneChange.StopOffset : The offset in ms to keep after a scene change (value set in the preferences dialog)
SceneChange.Visible : True if scene change are currenlty visible in VSS (value set in the preferences dialog)
SceneChange.GetCount() : Get the total number of scene change
SceneChange.GetAt(Index) : Get the time of the scene change at the specified index in ms. Index is between 0 and GetCount()-1
SceneChange.GetNext(TimeMs) : Get the time in ms of the next scene change superior or equal to TimeMs
SceneChange.GetPrevious(TimeMs) : Get the time in ms of the next scene change inferior or equal to TimeMs
SceneChange.Contains(Start,Stop) : Check if there is a scene change between [Start,Stop]


