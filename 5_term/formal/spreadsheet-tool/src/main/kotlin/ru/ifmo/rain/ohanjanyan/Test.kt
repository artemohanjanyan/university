package ru.ifmo.rain.ohanjanyan

import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.model.BatchUpdateSpreadsheetRequest
import com.google.api.services.sheets.v4.model.Request
import com.google.api.services.sheets.v4.model.SheetProperties
import com.google.api.services.sheets.v4.model.UpdateSheetPropertiesRequest
import java.util.*

fun main(args: Array<String>) {
    println("Kotlin works")

    val service: Sheets = SheetsQuickstart.getSheetsService()

    val spreadsheet = service.spreadsheets()
            .get("1acGfd2XLnB873zoXuWHkH551StJ6ThO1XdJ7Vqbn7NA")
            .setIncludeGridData(true)
            .execute()

    val archiveTmp = spreadsheet.sheets.first { it.properties.title == "АрхивTMP" }
    val requests = ArrayList<Request>()

    val sheetProperties = SheetProperties()
    sheetProperties.sheetId = archiveTmp.properties.sheetId
    sheetProperties.title = "Архив"
    val updateSheetPropertiesRequest = UpdateSheetPropertiesRequest()
    updateSheetPropertiesRequest.properties = sheetProperties
    updateSheetPropertiesRequest.fields = "*"
    requests.add(Request().setUpdateSheetProperties(updateSheetPropertiesRequest))

    service.spreadsheets()
            .batchUpdate(spreadsheet.spreadsheetId,
                    BatchUpdateSpreadsheetRequest().setRequests(requests))
            .execute()
}
