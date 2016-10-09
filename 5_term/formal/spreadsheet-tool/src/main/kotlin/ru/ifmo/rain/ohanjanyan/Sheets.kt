package ru.ifmo.rain.ohanjanyan

import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.model.*
import java.util.*

fun getRange(sheetName: String,
             rowStart: Int, columnStart: Int,
             height: Int, width: Int): String {

    fun intToColumn(i: Int): String {
        if (i <= 26) {
            return (i - 1 + 'A'.toInt()).toChar().toString()
        }
        val low = (i - 1) % 26 + 1
        val high = (i - 1) / 26
        return (high - 1 + 'A'.toInt()).toChar().toString() + (low - 1 + 'A'.toInt()).toChar()
    }

    return sheetName +
            "!" + intToColumn(columnStart) + rowStart +
            ":" + intToColumn(columnStart + width - 1) + (rowStart + height - 1)
}

class SheetStudentInfoProvider(val service: Sheets, val id1: String, val id2: String) : StudentInfoProvider {

    private val spreadsheet1: Spreadsheet by lazy {
        service.spreadsheets()
                .get(id1)
                .execute()
    }

    private val spreadsheet2: Spreadsheet by lazy {
        service.spreadsheets()
                .get(id2)
                .execute()
    }

    private fun getInfo(spreadsheet: Spreadsheet, headerHeight: Int,
                        mapBuilder: (List<List<Any>>) -> Map<String, StudentInfo>): Map<String, StudentInfo> {
        val resultSheet = spreadsheet.sheets[0]
        val groupNumbers = service.spreadsheets().values().get(
                spreadsheet.spreadsheetId,
                getRange(resultSheet.properties.title,
                        headerHeight + 1, 1,
                        resultSheet.properties.gridProperties.rowCount - headerHeight,
                        resultSheet.properties.gridProperties.columnCount)
        ).execute().getValues()
        return if (groupNumbers != null)
            mapBuilder(groupNumbers)
        else
            emptyMap()
    }

    private val lazyInfo: Map<String, StudentInfo> by lazy {
        val info1 = getInfo(spreadsheet1, 0, {
            it.associateBy({ it[0].toString() }, {
                StudentInfo(it[1].toString(), it[2].toString().toInt())
            })
        })
        val info2 = getInfo(spreadsheet2, 1, {
            it.associateBy({ it[1].toString() }, {
                StudentInfo(it[2].toString(), it[3].toString().toInt())
            })
        })
        info1 + info2
    }

    override fun getInfo(): Map<String, StudentInfo> {
        return lazyInfo
    }
}

class SheetsResultsProvider(val service: Sheets,
                            val id: String,
                            val studentInfoProvider: StudentInfoProvider) : ResultsProvider {

    val studentsStartRow = 5

    private val spreadsheet: Spreadsheet by lazy {
        service.spreadsheets()
                .get(id)
                .execute()
    }

    override fun getStudents(): List<Student> {
        val resultSheet = spreadsheet.sheets[0]
        val groupNumbers = service.spreadsheets().values().get(
                spreadsheet.spreadsheetId,
                getRange(resultSheet.properties.title,
                        studentsStartRow, 1,
                        resultSheet.properties.gridProperties.rowCount - studentsStartRow + 1, 7)
        ).execute()
        val info = studentInfoProvider.getInfo()
        return groupNumbers.getValues().map {
            Student(it[0].toString(),
                    it[1].toString().toInt(),
                    it[6].toString().toInt())
        }.map {
            it.copy(group = info[it.name]?.group ?: it.group)
        }
    }

    override fun getSolvedTasks(group: Int): List<Int> {
        val resultSheet = spreadsheet.sheets[2]
        val groupNumbers = service.spreadsheets().values().get(
                spreadsheet.spreadsheetId,
                getRange(resultSheet.properties.title,
                        1, 1,
                        resultSheet.properties.gridProperties.rowCount,
                        resultSheet.properties.gridProperties.columnCount)
        ).execute()
        val values = groupNumbers.getValues()
        val groupRow =
                if (values[0][1].toString()
                        .contains(group.toString())) 1
                else 2
        return values.subList(1, values.size).mapNotNull {
            if (it.size > groupRow && it[groupRow].toString().isNotEmpty())
                it[0].toString().toInt()
            else
                null
        }
    }
}

class SheetTicketService(val service: Sheets,
                         val id: String,
                         val resultsProvider: ResultsProvider,
                         val infoProvider: StudentInfoProvider) : TicketService {

//    private val spreadsheet: Spreadsheet by lazy {
//        service.spreadsheets()
//                .get(id)
//                .execute()
//    }

    private fun spreadsheet(): Spreadsheet {
        return service.spreadsheets()
                .get(id)
                .execute()
    }

    private fun makeHeaderRowData(newTasks: List<Int>, oldTasks: List<Int>): RowData {
        val nameCell = CellData().setUserEnteredValue(ExtendedValue().setStringValue("Имя"))
        val scoreCell = CellData().setUserEnteredValue(ExtendedValue().setStringValue("Класс"))
        val solvedCell = CellData().setUserEnteredValue(ExtendedValue().setStringValue("Сумма"))
        val newTasksCells = newTasks.map {
            CellData().setUserEnteredValue(ExtendedValue().setStringValue(it.toString()))
        }
        val oldTasksCells = oldTasks.map {
            CellData().setUserEnteredValue(ExtendedValue().setStringValue(it.toString()))
        }
        return RowData().setValues(listOf(nameCell, scoreCell, solvedCell) + newTasksCells + oldTasksCells)
    }

    override fun makeTicketPage(group: Int, newTasks: IntRange) {
        val students = resultsProvider.getStudents()
                .filter { it.group == group }
                .sortedBy { it.name }
        val solvedTasks = resultsProvider.getSolvedTasks(group).toSortedSet()
        val info = infoProvider.getInfo()
        val newNotSolvedTasks = newTasks.filter { !solvedTasks.contains(it) }
        val oldNotSolvedTasks = (1 until newTasks.first).filter { !solvedTasks.contains(it) }

        val requests: ArrayList<Request> = ArrayList()

        val oldSheet = spreadsheet().sheets.firstOrNull {
            it.properties.title == group.toString()
        }
        if (oldSheet != null) {
            val deleteSheetRequest = DeleteSheetRequest()
            deleteSheetRequest.sheetId = oldSheet.properties.sheetId
            requests.add(Request().setDeleteSheet(deleteSheetRequest))
        }

        val gridProperties = GridProperties()
        gridProperties.rowCount = students.size + 1
        gridProperties.columnCount = newNotSolvedTasks.size + oldNotSolvedTasks.size + 3
        gridProperties.frozenRowCount = 1
        gridProperties.frozenColumnCount = 3
        val sheetProperties = SheetProperties()
        sheetProperties.gridProperties = gridProperties
        sheetProperties.index = 39 - group
        sheetProperties.title = group.toString()
        val addSheetRequest = AddSheetRequest()
        addSheetRequest.properties = sheetProperties
        requests.add(Request().setAddSheet(addSheetRequest))

        service.spreadsheets()
                .batchUpdate(spreadsheet().spreadsheetId,
                        BatchUpdateSpreadsheetRequest().setRequests(requests))
                .execute()
        requests.clear()

        val gridRange = GridRange()
        gridRange.sheetId = spreadsheet().sheets
                .first { it.properties.title == group.toString() }
                .properties.sheetId
        gridRange.startRowIndex = 0
        gridRange.startColumnIndex = 0
        gridRange.endRowIndex = gridProperties.rowCount
        gridRange.endColumnIndex = gridProperties.columnCount
        val rows = listOf(makeHeaderRowData(newNotSolvedTasks, oldNotSolvedTasks)) +
                students.mapIndexed { i, student ->
                    val cells = ArrayList(Collections.nCopies(gridProperties.columnCount, CellData()))
                    cells[0] = CellData().setUserEnteredValue(ExtendedValue().setStringValue(student.name))
                    cells[1] = CellData().setUserEnteredValue(
                            ExtendedValue().setNumberValue(student.classScore.toDouble())
                    )
                    cells[2] = CellData().setUserEnteredValue(ExtendedValue().setFormulaValue(
                            "=SUM(" + getRange(
                                    sheetProperties.title,
                                    i + 2, 4,
                                    1, gridProperties.columnCount - 3
                            ) + ")"
                    ))
                    val booleanCondition = BooleanCondition()
                    booleanCondition.type = "NUMBER_EQ"
                    booleanCondition.setValues(listOf(ConditionValue().setUserEnteredValue("1")))
                    val dataValidationRule = DataValidationRule()
                    dataValidationRule.strict = true
                    dataValidationRule.condition = booleanCondition
                    for (j in 3 until cells.size) {
                        cells[j] = CellData().setDataValidation(dataValidationRule)
                    }
                    RowData().setValues(cells)
                }
        val updateCellsRequest = UpdateCellsRequest()
        updateCellsRequest.range = gridRange
        updateCellsRequest.rows = rows
        updateCellsRequest.fields = "*"
        requests.add(Request().setUpdateCells(updateCellsRequest))

        val frozenDimensionRange = DimensionRange()
        frozenDimensionRange.dimension = "COLUMNS"
        frozenDimensionRange.sheetId = gridRange.sheetId
        frozenDimensionRange.startIndex = 0
        frozenDimensionRange.endIndex = 3
        requests.add(Request().setAutoResizeDimensions(
                AutoResizeDimensionsRequest().setDimensions(frozenDimensionRange)))
        val tasksDimensionRange = DimensionRange()
        tasksDimensionRange.dimension = "COLUMNS"
        tasksDimensionRange.sheetId = gridRange.sheetId
        tasksDimensionRange.startIndex = 3
        tasksDimensionRange.endIndex = gridRange.endColumnIndex
        val updateDimensionPropertiesRequest = UpdateDimensionPropertiesRequest()
        updateDimensionPropertiesRequest.properties = DimensionProperties().setPixelSize(30)
        updateDimensionPropertiesRequest.fields = "*"
        updateDimensionPropertiesRequest.range = tasksDimensionRange
        requests.add(Request().setUpdateDimensionProperties(updateDimensionPropertiesRequest))

        val unprotectedRange = GridRange()
        unprotectedRange.sheetId = gridRange.sheetId
        unprotectedRange.startRowIndex = 1
        unprotectedRange.startColumnIndex = 3
        unprotectedRange.endRowIndex = gridRange.endRowIndex
        unprotectedRange.endColumnIndex = gridRange.endColumnIndex
        val sheetRange = GridRange()
        sheetRange.sheetId = gridRange.sheetId
        val protectedRange = ProtectedRange()
        protectedRange.range = sheetRange
        protectedRange.editors = Editors().setUsers(emptyList())
        protectedRange.unprotectedRanges = listOf(unprotectedRange)
        val addProtectedRangeRequest = AddProtectedRangeRequest()
        addProtectedRangeRequest.protectedRange = protectedRange
        requests.add(Request().setAddProtectedRange(addProtectedRangeRequest))

        for (student in students.zip(1 until gridRange.endRowIndex)) {
            val email = info[student.first.name]?.email ?: continue
            val studentRange = GridRange()
            studentRange.sheetId = gridRange.sheetId
            studentRange.startRowIndex = student.second
            studentRange.startColumnIndex = 3
            studentRange.endRowIndex = student.second + 1
            studentRange.endColumnIndex = gridRange.endColumnIndex
            val studentProtectedRange = ProtectedRange()
            studentProtectedRange.range = studentRange
            studentProtectedRange.editors = Editors().setUsers(listOf(email))
            val addStudentProtectedRangeRequest = AddProtectedRangeRequest()
            addStudentProtectedRangeRequest.protectedRange = studentProtectedRange
            requests.add(Request().setAddProtectedRange(addStudentProtectedRangeRequest))
        }

        // todo conditional formatting
        // TODO Request().setUpdateBorders

        service.spreadsheets()
                .batchUpdate(spreadsheet().spreadsheetId,
                        BatchUpdateSpreadsheetRequest().setRequests(requests))
                .execute()
    }

    override fun makeSummaryPage(groups: List<Pair<Int, IntRange>>) {
        throw UnsupportedOperationException("not implemented")
    }

    override fun share(emails: List<String>) {
        throw UnsupportedOperationException("not implemented")
    }
}
