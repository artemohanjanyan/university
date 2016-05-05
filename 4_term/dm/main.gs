//var textUpdateDialog = "Обновить ДЗ";
//var textUpdateAccess = "Обновить доступ";

var ui = SpreadsheetApp.getUi();
var spreadsheet = SpreadsheetApp.getActiveSpreadsheet();

//function onOpenOld() {
//  spreadsheet.addMenu("АСД",
//      [{name : textUpdateDialog, functionName : "onUpdateDialog"},
//       {name : textUpdateAccess, functionName : "onUpdateAccess"}]);
//  
//  //debug
//  //update(1, 10);
//}

function onUpdateDialog() {
  var result = ui.prompt("Обновить ДЗ", "Диапазон заданий:", ui.ButtonSet.OK_CANCEL);
  
  var button = result.getSelectedButton();
  var text = result.getResponseText();
  if (button == ui.Button.OK) {
    var dashPos = text.search('-');
    var start = parseInt(text.substr(0, dashPos));
    var end = parseInt(text.substr(dashPos + 1, text.length));
    if (!isNaN(start) && !isNaN(end) && start <= end && end - start <= 30) {
      update(start, end);
    } else {
      ui.alert("Incorrect range");
    }
  }
}

function test() {
  //update(39, 51);
  
  updateSummary(spreadsheet.getSheetByName("39"), spreadsheet.getSheetByName("37"), 
      spreadsheet.getSheetByName("tmp 37 39"), spreadsheet.getSheetByName("Summary 37 39"));
  updateSummary(spreadsheet.getSheetByName("38"), spreadsheet.getSheetByName("36"), 
      spreadsheet.getSheetByName("tmp 36 38"), spreadsheet.getSheetByName("Summary 36 38"));
}

function update(start, end) {
  var groupColumn = [0, 1, 0, 1];
  var sheets = spreadsheet.getSheets();
  for (var i = 0; i < 4; ++i) {
    var sheet = sheets[i];
    var tasks = getTaskArray(start, end, groupColumn[i]);
    
    if (sheet.getMaxColumns() > 1) {
      sheet.deleteColumns(2, sheet.getMaxColumns() - 1);
    }
    sheet.insertColumnsAfter(1, tasks.length);
    
    for (var j = 2; j <= sheet.getMaxColumns(); ++j) {
      sheet.setColumnWidth(j, 40);
    }
    
    var lastNew = end - start + 1;
    for (var j = 0; j < tasks.length - 1; ++j) {
      if (tasks[j] > tasks[j + 1]) {
        lastNew = j + 1;
        break;
      }
    }
    
    var rangeA = sheet.getRange(3, 2, sheet.getMaxRows() - 2, lastNew);
    var rangeB = sheet.getRange(3, 2 + lastNew, sheet.getMaxRows() - 2, sheet.getMaxColumns() - (1 + lastNew));
    rangeA.setBorder(true, true, true, true, false, false);
    rangeB.setBorder(true, true, true, true, false, false);
    
    var dataValidation = SpreadsheetApp.newDataValidation()
        .requireNumberEqualTo(1)
        .setAllowInvalid(false)
        .build();
    rangeA.setDataValidation(dataValidation);
    rangeB.setDataValidation(dataValidation);
    
    rangeA.setHorizontalAlignment("right");
    rangeB.setHorizontalAlignment("right");
    
    rangeA = sheet.getRange(1, 2, 1, lastNew);
    rangeB = sheet.getRange(1, 2 + lastNew, 1, sheet.getMaxColumns() - (1 + lastNew));
    rangeA.merge();
    rangeB.merge();
    rangeA.setValue("Задачи");
    rangeB.setValue("Предыдущие");
    rangeA.setBorder(true, true, false, true, false, false);
    rangeB.setBorder(true, true, false, true, false, false);
    rangeA.setFontWeight("normal");
    rangeB.setFontWeight("normal");
    
    rangeA = sheet.getRange(2, 2, 1, lastNew);
    rangeB = sheet.getRange(2, 2 + lastNew, 1, sheet.getMaxColumns() - (1 + lastNew));
    rangeA.setBorder(false, true, true, true, false, false);
    rangeB.setBorder(false, true, true, true, false, false);
    rangeA.setFontWeight("normal");
    rangeB.setFontWeight("normal");
    
    rangeA = sheet.getRange(2, 2, 1, tasks.length);
    rangeA.setValues([tasks]);
    
    sheet.protect();
    var protection = sheet.getProtections(SpreadsheetApp.ProtectionType.SHEET)[0];
    protection.setUnprotectedRanges([sheet.getRange(3, 2, sheet.getMaxRows() - 2, sheet.getMaxColumns() - 1)]);
  }
}

function updateSummary(example1, example2, tmp, summary) {
  if (tmp.getMaxColumns() > 5) {
    tmp.deleteColumns(6, tmp.getMaxColumns() - 5);
  }
  tmp.insertColumnsAfter(5, example1.getMaxColumns() - 1);
  example1.getRange(1, 2, 2, example1.getMaxColumns() - 1).copyTo(tmp.getRange(1, 6));
  for (var columnI = 6; columnI <= tmp.getMaxColumns(); ++columnI) {
    tmp.setColumnWidth(columnI, 40);
  }
  
  tmp.getRange(3, 6).setFormula("'" + example1.getName() + "'!" + "B3");
  tmp.getRange(3, 6).copyTo(tmp.getRange(3, 6, example1.getMaxRows() - 2, example1.getMaxColumns() - 1));
  tmp.getRange(3 + example1.getMaxRows() - 2, 6).setFormula("'" + example2.getName() + "'!" + "B3");
  tmp.getRange(3 + example1.getMaxRows() - 2, 6)
      .copyTo(tmp.getRange(3 + example1.getMaxRows() - 2, 6, example2.getMaxRows() - 2, example2.getMaxColumns() - 1));
  
  var lastNew = 7;
  while (lastNew <= tmp.getMaxColumns() && tmp.getRange(2, lastNew).getValue() > tmp.getRange(2, lastNew - 1).getValue()) {
    ++lastNew;
  }
  tmp.getRange(3, 5).setFormula("=SUM(" + tmp.getRange(3, 6, 1, lastNew - 6).getA1Notation() + ")");
  tmp.getRange(3, 5).copyTo(tmp.getRange(3, 5, example1.getMaxRows() - 2 + example2.getMaxRows() - 2));
      
  tmp.getRange(1, 6, 2, example1.getMaxColumns() - 1).copyTo(summary.getRange(1, 6));
  for (var columnI = 6; columnI <= summary.getMaxColumns(); ++columnI) {
    summary.setColumnWidth(columnI, 30);
  }
  summary.getRange(3, 1).setFormula("SORT('" + tmp.getName() + "'!"
      + tmp.getRange(3, 1, example1.getMaxRows() - 2 + example2.getMaxRows() - 2, tmp.getMaxColumns()).getA1Notation()
      + "; 5; FALSE;2;TRUE;1;TRUE)");
}

function getTaskArray(start, end, column) {
  var tasksSheet = spreadsheet.getSheetByName("Практика");
  // B2:B175
  var range = tasksSheet.getRange(2, 2 + column, 174);

  var ans = [];
  for (var i = start; i <= end; ++i) {
    if (range.getCell(i, 1).getDisplayValue() == "") {
      ans.push(i);
    }
  }

  for (var i = 1; i < start; ++i) {
    if (range.getCell(i, 1).getDisplayValue() == "") {
      ans.push(i);
    }
  }

  return ans;
}

function onUpdateAccess() {
  var emails = getEmails();
}

function getEmails() {
  var emails = [];

  var baseSS = SpreadsheetApp.openById("1kC5yf9jEWqXbMmyiqAp09NNucm1yTLBNR9jQpqsuet8");

  for (var i = 0; i < 2; ++i) {
    var thatSheet = baseSS.getSheets()[i];
    var protections = thatSheet.getProtections(SpreadsheetApp.ProtectionType.RANGE);
    for (var j = 0; j < protections.length; ++j) {
      var range = protections[j].getRange();
      if (range.getRow() == 1 || protections[j].getEditors().length < 2) {
        continue;
      }
      //emails.push([thatSheet.getRange(range.getRow(), range.getColumn()).getValue(), protections[j].getEditors()[1].getEmail()]);
      emails[thatSheet.getRange(range.getRow(), range.getColumn()).getValue()] = protections[j].getEditors()[1].getEmail();
    }
  }
  
  baseSS = SpreadsheetApp.openById("1TU4sCD1PYR4BVMkxpnz0jWLF3oC4GmNZieDzX83PJHE");
  var thatSheet = baseSS.getSheets()[0];
  var thatRange = thatSheet.getRange(2, 3, thatSheet.getLastRow() - 1, 2);
  var values = thatRange.getValues();
  for (var i = 0; i < values.length; ++i) {
    //emails.push(values[i]);
    emails[values[i][0]] = values[i][1];
  }
  
  return emails;
}

function resetRangeProtections() {
  var emails = getEmails();
  provideAccess(emails);

  var sheets = spreadsheet.getSheets();
  
  for (var name in emails) {
    if (emails.hasOwnProperty(name)) {
      spreadsheet.addEditor(emails[name]);
    }
  }
  
  for (var i = 0; i < 4; ++i) {
    sheet = sheets[i];
    var oldProtections = sheet.getProtections(SpreadsheetApp.ProtectionType.RANGE);
    for (var j = 0; j < oldProtections.length; ++j) {
      oldProtections[j].remove();
    }
    
    for (var j = 3; j <= sheet.getLastRow(); ++j) {
      var range = sheet.getRange(j, 2, 1, sheet.getLastColumn() - 1)
      var protection = range.protect();
      protection.removeEditors(protection.getEditors());
      var name = sheet.getRange(j, 1).getValue();
      if (name in emails) {
        protection.addEditor(emails[name]);
      }
    }
  }
}

function provideAccess(emails) {
  for (var name in emails) {
    if (emails.hasOwnProperty(name)) {
      spreadsheet.addEditor(emails[name]);
    }
  }
}
