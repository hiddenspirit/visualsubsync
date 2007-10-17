// ---------------------------------------------------------------------------

// Round a number to 1 decimal
// eg: 1.23 => 1.2
function decimal1Round(avalue) {
  return Math.round(avalue * 10) / 10;
}

// ---------------------------------------------------------------------------

function getReadingSpeed(Sub) {
  var len = Sub.StrippedText.length;
  var durMs = Sub.Stop - Sub.Start;
  if (durMs < 500) {
    durMs = 500;
  }
  var rs = (len * 1000) / (durMs - 500);
  return rs;
}

// ---------------------------------------------------------------------------

function getReadingSpeedRating(rs) {
    // rating
    var rating;
    if      (rs < 5)  return "TOO SLOW!";
    else if (rs < 10) return "Slow, acceptable.";
    else if (rs < 13) return "A bit slow.";
    else if (rs < 15) return "Good.";
    else if (rs < 23) return "Perfect.";
    else if (rs < 27) return "Good.";
    else if (rs < 31) return "A bit fast.";
    else if (rs < 35) return "Fast, acceptable.";
    else              return "TOO FAST!";
}

// ---------------------------------------------------------------------------

