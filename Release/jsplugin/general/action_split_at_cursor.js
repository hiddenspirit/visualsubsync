// ---------------------------------------------------------------------------

JSAction_SplitAtCursor = {
  onExecute : function() {
    var selectionStart = VSSCore.GetTextSelectionStart();
    if (selectionStart < 1) {
        return;
    }
    var selectedSub = VSSCore.GetFirstSelected();
    if (selectedSub.Text.length - selectionStart < 1) {
        return;
    }
    var newText = selectedSub.Text; 
    newText = newText.substring(0, selectionStart).replace(/\s*\r\n\s*/mg, ' ')    
        + '\r\n'
        + newText.substring(selectionStart).replace(/\s*\r\n\s*/mg, ' ');
    selectedSub.Text = newText;
  }
};

VSSCore.RegisterJavascriptAction('JSAction_SplitAtCursor', 'Split at cursor', '');

// ---------------------------------------------------------------------------