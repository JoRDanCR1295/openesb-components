/*
 * $Id: TestSpreadsheetTable.java,v 1.1 2008/10/09 05:08:40 jawed Exp $
 * =======================================================================
 * Copyright (c) 2002 Axion Development Team.  All rights reserved.
 *  
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * 1. Redistributions of source code must retain the above 
 *    copyright notice, this list of conditions and the following 
 *    disclaimer. 
 *   
 * 2. Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in 
 *    the documentation and/or other materials provided with the 
 *    distribution. 
 *   
 * 3. The names "Tigris", "Axion", nor the names of its contributors may 
 *    not be used to endorse or promote products derived from this 
 *    software without specific prior written permission. 
 *  
 * 4. Products derived from this software may not be called "Axion", nor 
 *    may "Tigris" or "Axion" appear in their names without specific prior
 *    written permission.
 *   
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * =======================================================================
 */

package org.axiondb.engine.tables;

import java.io.File;
import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.NoSuchElementException;
import java.util.Random;

import junit.framework.Test;
import junit.framework.TestSuite;
import jxl.Workbook;
import jxl.WorkbookSettings;
import jxl.write.Label;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;
import org.axiondb.AxionException;
import org.axiondb.Column;
import org.axiondb.ColumnIdentifier;
import org.axiondb.Database;
import org.axiondb.ExternalTable;
import org.axiondb.ExternalTableLoader;
import org.axiondb.Index;
import org.axiondb.Literal;
import org.axiondb.Row;
import org.axiondb.RowIterator;
import org.axiondb.Table;
import org.axiondb.TableIdentifier;
import org.axiondb.constraints.PrimaryKeyConstraint;
import org.axiondb.engine.DiskDatabase;
import org.axiondb.engine.commands.CreateViewCommand;
import org.axiondb.engine.rows.SimpleRow;
import org.axiondb.types.CharacterVaryingType;
import org.axiondb.types.IntegerType;
import org.axiondb.types.LOBType;
import org.axiondb.types.ObjectType;
import org.axiondb.types.BigDecimalType;
import org.axiondb.constraints.NotNullConstraint;
import org.axiondb.constraints.NullConstraint;
import org.axiondb.engine.commands.AddConstraintCommand;
import org.axiondb.io.FileUtil;
import org.axiondb.types.BooleanType;
import org.axiondb.engine.commands.AlterTableCommand;

/**
 * @version $Revision: 1.1 $ $Date: 2008/10/09 05:08:40 $
 * @author Ahimanikya Satapathy
 * @author jawed
 */
public class TestSpreadsheetTable extends AbstractTableTest {

    //------------------------------------------------------------ Conventional

    public TestSpreadsheetTable(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestSpreadsheetTable.class);
        return suite;
    }

    //--------------------------------------------------------------- Lifecycle

    protected DiskDatabase _db = null;
    protected String tableName = null;
    protected String dataFileName = null;

    protected Table createTable(String name) throws Exception {
        tableName = name;
        ExternalTableLoader loader = new SpreadsheetTableLoader();
        ExternalTable t = (ExternalTable) loader.createTable(_db, name);
        t.loadExternalTable(setProperties(name));
        return t;
    }
    
    protected Database getDatabase() throws Exception {
        return _db;
    }
    
    protected File getDataFile() throws Exception {
        return new File(dataFileName);
    }

    protected Properties setProperties(String name) {
        Properties props = new Properties();

        props.setProperty(ExternalTable.PROP_LOADTYPE, "spreadsheet");
        props.setProperty(SpreadsheetTable.PROP_ISFIRSTLINEHEADER, "false");
        props.setProperty(SpreadsheetTable.PROP_ROWSTOSKIP, "0");
        dataFileName = getDbdir().getAbsolutePath()+"\\"+name + ".xls";
        props.setProperty(SpreadsheetTable.PROP_FILENAME, dataFileName);
        props.setProperty(SpreadsheetTable.PROP_SHEET, "Sheet1");

        return props;
    }
    
    protected String getTableName() {
        return tableName != null ? tableName : "FFCSV";
    }
    
    public void setUp() throws Exception {
        getDbdir().mkdirs();
        _db = new DiskDatabase(getDbdir());
        super.setUp();
    }

    public void tearDown() throws Exception {
        super.tearDown();
        _db.shutdown();
        File data = new File(getDbdir(), tableName + ".xls");
        data.delete();
    }

    //------------------------------------------------------------------- Tests

    public void testObjectTable() throws Exception {
        // TODO: Make this test pass, define a interface MarshallableObject for method
        // toString and toObject, or or MarshallableXMLObject toXMLString , toObject , If
        // the Object implement this then we can use them in flat file.
    }
    
    public void testInvalidPropertyKey() throws Exception {
        try {
            Properties badProps = new Properties();
            badProps.put(ExternalTable.PROP_LOADTYPE, "spreadsheet");
            badProps.put("UNKNOWN_PROPERTY", Boolean.TRUE);
            ExternalTableFactory factory = new ExternalTableFactory();
            factory.createTable(_db, "BadTable", badProps, buildColumns());
            fail("Expected AxionException due to unrecognized property name 'UNKNOWN_PROPERTY'");
        } catch (AxionException expected) {
            // Expected AxionException due to unrecognized property name.
        }
    }
    
    public void testDiskInsert() throws Exception {
        testAddRow();
        table.shutdown();
        File data = new File(dataFileName);
        WritableWorkbook writeableWorkbook = Workbook.createWorkbook(data,getWorkbookSettings());
        WritableSheet writeableSheet = writeableWorkbook.createSheet("Sheet1", 0);
        writeableWorkbook.write();
        writeableWorkbook.close();
        assertTrue("Should have data file", data.exists());
        //assertTrue("Should have some data in data file", data.length() >= 12);
    }
    
    public void testInvalidDataType() throws Exception {
        try {
            table.addColumn(new Column("LOBCOL", new LOBType()));
        } catch(UnsupportedOperationException e) {
            // expected
            fail("Expected Exception");
            
        }
        
        try {
            table.addColumn(new Column("OBJCOL", new ObjectType()));
        } catch(UnsupportedOperationException e) {
            // expected
            fail("Expected Exception");
        }
    }
    
    public void testRemount() throws Exception {
        testAddRow();
        ((ExternalTable)table).remount();

        RowIterator iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertTrue(iter.hasNext());
        assertNotNull(iter.next());
        assertTrue(iter.hasNext());
        assertNotNull(iter.next());
        assertTrue(!iter.hasNext());
    }

    public void testDiskDrop() throws Exception {
        testAddRow();
        File tabledir = new File(getDbdir(), getTableName());
        File meta = new File(tabledir, getTableName() + ".META");
        assertTrue("Table directory should exist", tabledir.exists());
        assertTrue("Meta file should exist", meta.exists());
        table.drop();
        assertTrue("Meta file should not exist", !meta.exists());
        assertTrue("Table directory should not exist", !tabledir.exists());
    }
    
//    public void testFileReadCustomRecordDelimiter() throws Exception {
//        File data = new File(getDbdir(), "FFTEST.csv");
//        FileWriter out = new FileWriter(data);
//
//        String eol = "AA##BB";
//
//        out.write("\"ID\", \"NAME\"" + eol); // Header
//        out.write("\"1\",\"aaaa\"" + eol); // 1
//        out.write("\"we\"," + eol); // bad 1
//        out.write("\"2\",\"bbbb\"" + eol); // 2
//        out.write("\"3\",\"cccc\"" + eol); // 3
//        out.write("\"4\",\"dddd\"" + eol); // 4
//        out.write("\"xx\",\"xx\"" + eol); // bad 2
//        out.write("\"5\",\"sfdf\"" + eol); // 5
//        out.write("\"6\",\"eeee\"" + eol); // 6
//        out.write("\"2004-10-10\",\"hhhh\"" + eol); // bad 3
//        out.write("\"7.0\", \"AAc\"" + eol); // 7
//        out.close();
//
//        ExternalTableFactory factory = new ExternalTableFactory();
//        Properties prop = setProperties("FFTEST");
//        prop.put(BaseFlatfileTable.PROP_ISFIRSTLINEHEADER, "true");
//        prop.put(DelimitedFlatfileTable.PROP_QUALIFIER, "\"");
//        prop.put(BaseFlatfileTable.PROP_RECORDDELIMITER, "AA##BB");
//        
//        Table table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//        RowIterator itr = table2.getRowIterator(false);
//
//        int rowCount = 0;
//        while (itr.hasNext()) {
//            itr.next();
//            rowCount++;
//        }
//
//        assertEquals("Valid row count should have correct value", 7, rowCount);
//        table2.drop();
//        data.delete();
//    }  
    
//    public void testGetLobDir() {
//        try {
//            ((DelimitedFlatfileTable)table).getLobDir();
//            fail("Expected Exception");
//        }catch(UnsupportedOperationException e) {
//            // expected
//        }  
//    }
    
//    public void testFileReadQuotedMissingCloseQuote() throws Exception {
//        final String ffName = "QuotedTest";
//        
//        File data = new File(getDbdir(), ffName + ".csv");
//        FileWriter out = new FileWriter(data);
//        
//        String eol = System.getProperty("line.separator");
//        out.write("\"ID\", \"NAME\"" + eol); // Header
//        out.write("\"1\",\"dfdf" + eol); // 5 - missing close quote
//        out.close();
//
//        Table table2 = null;
//        try {
//            ExternalTableFactory factory = new ExternalTableFactory();
//            Properties prop = setProperties("FFTEST");
//            prop.put(BaseFlatfileTable.PROP_ISFIRSTLINEHEADER, "true");
//            prop.put(DelimitedFlatfileTable.PROP_QUALIFIER, "\"");
//            prop.put(BaseFlatfileTable.PROP_RECORDDELIMITER, eol);
//            prop.setProperty(BaseFlatfileTable.PROP_FILENAME, data.getCanonicalPath());
//            
//            table2 = factory.createTable(_db, ffName, prop, buildColumns());
//            RowIterator itr = table2.getRowIterator(false);
//    
//            int rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//                Row row = itr.current();
//                assertEquals(new BigDecimal(1), row.get(0));
//                assertEquals("dfdf", row.get(1));
//            }
//    
//            assertEquals("Valid row count should have correct value", 1, rowCount);
//        } finally {
//            if (table2 != null) {
//                table2.drop();
//            }
//            data.delete();
//        }
//    }
    
//    public void testFileReadQuoted() throws Exception {
//        final String ffName = "QuotedTest";
//        
//        File data = new File(getDbdir(), ffName + ".csv");
//        FileWriter out = new FileWriter(data);
//
//        String eol = System.getProperty("line.separator");
//        out.write("\"ID\", \"NAME\"" + eol); // Header
//        out.write("\"1\", \"aa\"" + eol); // 1
//        out.write("\"2.00\", \"bbb\"" + eol); // 2
//        out.write("\"3.00\"  , \"ccc\"" + eol); // 3
//        out.write("\"4.00\", \"\"" + eol); // 4
//        out.write("" + eol);    // skip
//        out.write("\"we\"," + eol); // bad 1
//        out.write("\"7.0f\", \"ccc\"" + eol); // bad 3 ?
//        out.write("\"xx\",\"xx\"" + eol); // bad 4
//        out.write("\"2004-10-10\",\"hhhh\"" + eol); // bad 5
//        out.write("\"7\",\"dfdf" + eol); // 5 - missing end quote
//        out.write("\"\"  , \"\"" + eol); // 6
//        
//        String name = "";
//        for(int i = 0 ; i < 20 ; i++) {
//            name += "cccdd";
//        }
//        out.write("3.00," + name + ""); // 7 - no quotes
//        out.close();
//
//        
//        Table table2 = null;
//        try {
//            ExternalTableFactory factory = new ExternalTableFactory();
//            Properties prop = setProperties("FFTEST");
//            prop.put(BaseFlatfileTable.PROP_ISFIRSTLINEHEADER, "true");
//            prop.put(DelimitedFlatfileTable.PROP_QUALIFIER, "\"");
//            prop.put(BaseFlatfileTable.PROP_RECORDDELIMITER, eol);
//            prop.setProperty(BaseFlatfileTable.PROP_FILENAME, data.getCanonicalPath());
//            
//            table2 = factory.createTable(_db, ffName, prop, buildColumns());
//            RowIterator itr = table2.getRowIterator(false);
//    
//            int rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//        } finally {
//            if (table2 != null) {
//                table2.drop();
//            }
//            data.delete();
//        }
//    }
    
    private WorkbookSettings getWorkbookSettings() {
        WorkbookSettings settings = new WorkbookSettings();
        settings.setDrawingsDisabled(true);
        settings.setAutoFilterDisabled(true);
        settings.setSuppressWarnings(true);
        settings.setNamesDisabled(true);
        settings.setIgnoreBlanks(true);
        settings.setCellValidationDisabled(true);
        settings.setFormulaAdjust(false);
        settings.setPropertySets(false);
        return settings;
    }
    public void testFileReadNoValidRow() throws Exception {
        File data = new File(getDbdir(), "FFTest.xls");

        WritableWorkbook writeableWB = Workbook.createWorkbook(data, getWorkbookSettings());
        WritableSheet wSheet = writeableWB.createSheet("Sheet1", 0);
        
        Label label = new Label(0, 0, "ID");
        wSheet.addCell(label);
        label = new Label(1, 0, "NAME");
        wSheet.addCell(label);
        label = new Label(0, 1, "aa");
        wSheet.addCell(label);
        label = new Label(1, 1, "aa");
        wSheet.addCell(label);
        label = new Label(0, 2, "bbb");
        wSheet.addCell(label);
        label = new Label(1, 2, "bbb");
        wSheet.addCell(label);
        label = new Label(0, 3, "ccc");
        wSheet.addCell(label);
        label = new Label(1, 3, "ccc");
        wSheet.addCell(label);
        writeableWB.write();
        writeableWB.close();

        ExternalTableFactory factory = new ExternalTableFactory();
        Properties prop = setProperties("FFTEST");
        prop.put(SpreadsheetTable.PROP_ISFIRSTLINEHEADER, "true");
        
        Table table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
        RowIterator itr = table2.getRowIterator(false);

        int rowCount = 0;
        while (itr.hasNext()) {
            itr.next();
            rowCount++;
        }
        assertEquals("Valid row count should have correct value", 0, rowCount);
        table2.drop();

        table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
        itr = table2.getRowIterator(false);

        _db.addTable(table2);
        
        CreateViewCommand cmd = new CreateViewCommand();
        cmd.setObjectName("FFTESTVIEW");
        cmd.setIfNotExists(true);
        cmd.setSubQuery("select * from FFTest");
        cmd.execute(_db);
        TableView view = (TableView)_db.getTable("FFTESTVIEW");
        assertEquals("Valid row count should have correct value", 0, view.getRowCount());
        
        table2.drop();
        data.delete();
    }  

//    public void testFileRead() throws Exception {
//        File data = new File(getDbdir(), "FFTEST.csv");
//        FileWriter out = new FileWriter(data);
//
//        String eol = System.getProperty("line.separator");
//
//        out.write("ID, NAME" + eol); // Header
//        out.write("1, aa" + eol); // 1
//        out.write("2.00, bbb" + eol); // 2
//        out.write("3.00, ccc" + eol); // 3
//        out.write("4.00, ddd" + eol); // 4
//        out.write("" + eol);    // skip
//        out.write("we," + eol); // bad 1
//        out.write("7,dfdf" + eol); // 5
//        out.write("7.0f, ccc" + eol); // bad 3 ?
//        out.write("xx,xx" + eol); // bad 4
//        out.write("5, test" + eol); // 6
//        out.write("2004-10-10,hhhh" + eol); // bad 5
//        out.write("" + eol); // skip
//        
//        String name = "";
//        for(int i = 0 ; i < 20 ; i++) {
//            name += "cccdd";
//        }
//        out.write("3.00," + name + eol); // 7
//        out.write("" + eol); // skip
//        out.close();
//
//        try {
//            ExternalTableFactory factory = new ExternalTableFactory();
//            Properties prop = setProperties("FFTEST");
//            prop.put(BaseFlatfileTable.PROP_ISFIRSTLINEHEADER, "true");
//            prop.put(BaseFlatfileTable.PROP_ROWSTOSKIP, "1");
//            
//            Table table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//            RowIterator itr = table2.getRowIterator(false);
//    
//            int rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//            table2.drop();
//            
//            
//            prop.put(BaseFlatfileTable.PROP_MAXFAULTS, "2");
//            
//            try {
//                table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//                itr = table2.getRowIterator(false);
//                while (itr.hasNext()) {
//                    itr.next();
//                }
//                fail("Expected Exception");
//            } catch(Exception e) {
//                // expected
//            }
//            table2.drop();
//            
//            // bad property value, shd use default value
//            prop.put(BaseFlatfileTable.PROP_MAXFAULTS, "-10"); 
//            prop.put(BaseFlatfileTable.PROP_ROWSTOSKIP, "-10");
//            table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//            itr = table2.getRowIterator(false);
//    
//            rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//            table2.drop();
//            
//            // bad property value, shd use default value        
//            prop.put(BaseFlatfileTable.PROP_MAXFAULTS, "ABC");
//            prop.put(BaseFlatfileTable.PROP_ROWSTOSKIP, "ABC");
//            table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//            itr = table2.getRowIterator(false);
//    
//            rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//            table2.drop();
//    
//            prop.put(BaseFlatfileTable.PROP_MAXFAULTS, "6");
//            prop.put(BaseFlatfileTable.PROP_ROWSTOSKIP, "0");
//            table2 = factory.createTable(_db, "FFTEST", prop, buildColumns());
//            itr = table2.getRowIterator(false);
//
//    
//            rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//            
//            _db.addTable(table2);
//            CreateTableCommand cmd = new CreateTableCommand();
//            cmd.setObjectName("FFTEST2");
//            cmd.setType("external");
//            prop.put(BaseFlatfileTable.PROP_FILENAME, "FFTest2.csv");
//            prop.put(BaseFlatfileTable.PROP_ISFIRSTLINEHEADER, "true");
//            cmd.setProperties(prop);
//            AxionQueryContext ctx = new AxionQueryContext();
//            ctx.addSelect(new ColumnIdentifier("*"));
//            ctx.addFrom(new TableIdentifier("FFTEST"));
//            SubSelectCommand subSelect = new SubSelectCommand(ctx);
//            cmd.setSubQuery(subSelect);
//            cmd.execute(_db);
//            
//            Table table3 = _db.getTable("FFTEST2");
//            itr = table3.getRowIterator(false);
//            
//            assertEquals("Valid row count should have correct value", 7, table3.getRowCount());
//    
//            rowCount = 0;
//            while (itr.hasNext()) {
//                itr.next();
//                rowCount++;
//            }
//    
//            assertEquals("Valid row count should have correct value", 7, rowCount);
//        } finally {
//            try {
//                _db.dropTable("FFTEST");
//            } catch (AxionException ignore) {
//                // ignore
//            }
//            
//            try {
//                _db.dropTable("FFTEST2");
//            } catch (AxionException ignore) {
//                // ignore
//            }
//    
//            if (data != null) {
//                data.delete();
//            }
//        }
//    }

    public void testRestartDB() throws Exception {
        testAddRow();
        table.shutdown();
        table = null;
        _db.shutdown();

        _db = new DiskDatabase(getDbdir());
        assertTrue(_db.hasTable(getTableName()));
        table = _db.getTable(getTableName());
        testGetName();
        RowIterator iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertTrue(iter.hasNext());
        assertNotNull(iter.next());
        assertTrue(iter.hasNext());
        assertNotNull(iter.next());
        assertTrue(!iter.hasNext());
    }

    private List buildColumns() {
        List list = new ArrayList(2);
        list.add(new Column("ID", new IntegerType()));
        list.add(new Column("NAME", new CharacterVaryingType(100)));
        return list;
    }
    
    // -------- Test Cases copied from AbstractTableTest class --------------
    
     public void testGetName() throws Exception {
        assertEquals(getTableName().toUpperCase(), table.getName());
    }

    public void testToString() throws Exception {
        assertNotNull(table.getName());
        RowIterator iter = table.getRowIterator(false);
        assertNotNull(iter.toString());
    }

    public void testAddThenDropConstraint() throws Exception {
        addColumns();
        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
        addRows();
        Database db = getDatabase();
        db.addTable(table);
        PrimaryKeyConstraint pk = new PrimaryKeyConstraint("PK_FOO");
        pk.addSelectable(new ColumnIdentifier(new TableIdentifier(table.getName()), "ID"));
        table.addConstraint(pk);
        Column column = table.getColumn("ID");
        if (!table.isColumnIndexed(column)) {
            assertFalse(table.hasIndex("BOGUS"));
            Index index1 = db.getIndexFactory("btree").makeNewSystemInstance(table, column, db.getDBDirectory() == null);
            Index index2 = db.getIndexFactory("array").makeNewInstance("INDEX_FOO", column, true, db.getDBDirectory() == null);
            db.addIndex(index1, table, true);
            assertTrue(table.hasIndex(index1.getName()));
            db.addIndex(index2, table, true);
            assertTrue(table.hasIndex(index2.getName()));
        }

        try {
            table.addConstraint(new PrimaryKeyConstraint("PK_BAR"));
            fail("Expected AxionException");
        } catch (AxionException e) {
            // expected
        }
        try {
            table.addConstraint(new NotNullConstraint("PK_FOO"));
            fail("Expected AxionException");
        } catch (AxionException e) {
            // expected
        }

        table.addConstraint(new NotNullConstraint("NN_FOO"));
        table.removeConstraint("this constraint does not exist");
        table.removeConstraint("PRIMARYKEY");
        table.addConstraint(pk);
        table.removeConstraint("PK_FOO");
        table.removeConstraint("primarykey"); // shd be silent

        table.addConstraint(pk);
        table.removeConstraint("NN_FOO");
        table.addConstraint(new NullConstraint("N_FOO"));
        table.removeConstraint("N_FOO");
        table.removeConstraint(null);
        db.dropTable(table.getName());
        table = null;
    }

    public void testAddThenDropColumn() throws Exception {
//        table.addColumn(new Column("ID", new BigDecimalType()));
//        table.addColumn(new Column("NAME", new CharacterVaryingType(3)));
//        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
//        addRows();
//        Database db = getDatabase();
//        db.addTable(getTable());
//
//        PrimaryKeyConstraint pk = new PrimaryKeyConstraint("PK_FOO");
//        pk.addSelectable(new ColumnIdentifier(new TableIdentifier(getTableName()), "ID", null,
//            new BigDecimalType()));
//
//        AddConstraintCommand addCCmd = new AddConstraintCommand(getTableName(), pk);
//        addCCmd.execute(db);
//
//        AlterTableCommand alterCmd = new AlterTableCommand(table.getName(), false);
//        alterCmd.addColumn("NEWCOL", "varchar", "4", "0", new Literal("Test"), null);
//        alterCmd.execute(db);
//        table = db.getTable(getTableName());
//        assertTrue(table.hasColumn(new ColumnIdentifier("NEWCOL")));
//        assertNotNull(table.getConstraints().next());
//        RowIterator iter = table.getRowIterator(true);
//        assertNotNull(iter);
//        assertTrue(iter.hasNext());
//        Row row = iter.next();
//        assertEquals("Test", row.get(2));
//        assertTrue(iter.hasNext());
//        row = iter.next();
//        assertEquals("Test", row.get(2));
//        assertTrue(!iter.hasNext());
//
//        alterCmd = new AlterTableCommand(table.getName(), false);
//        alterCmd.dropColumn("NEWCOL");
//        alterCmd.execute(db);
//        table = db.getTable(getTableName());
//        assertFalse(table.hasColumn(new ColumnIdentifier("NEWCOL")));
//        iter = table.getRowIterator(true);
//        assertNotNull(iter);
//        assertTrue(iter.hasNext());
//        row = iter.next();
//        assertEquals("one", row.get(1));
//        assertTrue(iter.hasNext());
//        row = iter.next();
//        assertEquals("two", row.get(1));
//        assertTrue(!iter.hasNext());
//
//        try {
//            alterCmd = new AlterTableCommand("BOGUS", false);
//            alterCmd.dropColumn("NEWCOL");
//            alterCmd.execute(db);
//            fail("Expected Exception - table does not exist");
//        } catch (AxionException e) {
//            // expected table does not exist
//        }
//        
//        try {
//            alterCmd = new AlterTableCommand(table.getName(), false);
//            alterCmd.dropColumn("NEWCOL"); // does not exist
//            alterCmd.execute(db);
//            fail("Expected Exception - Bad column to drop");
//        } catch (AxionException e) {
//            // expected table does not exist
//        }
//
//        try {
//            AlterTableCommand cmd = new AlterTableCommand("FOO", false);
//            cmd.executeQuery(db);
//            fail("Expected UnsupportedOperationException");
//        } catch (UnsupportedOperationException e) {
//            // expected
//        }
    }

    public void testGetMatchingRowsForNull() throws Exception {
        RowIterator iter = table.getMatchingRows(null, null, true);
        assertNotNull(iter);
    }

    public void testHasColumn() throws Exception {
        ColumnIdentifier id = new ColumnIdentifier("FOO");
        assertTrue("Should not have column", !table.hasColumn(id));
        try {
            table.getColumnIndex("FOO");
            fail("Expected AxionException");
        } catch (AxionException e) {
            // expected
        }
        table.addColumn(new Column("FOO", new CharacterVaryingType(10)));
        assertTrue("Should have column", table.hasColumn(id));
        id.setTableIdentifier(new TableIdentifier(getTableName()));
        assertTrue("Should have column", table.hasColumn(id));

        id.setTableIdentifier(new TableIdentifier("BOGUS"));
        assertTrue("Should not have column", !table.hasColumn(id));
    }

    protected void addColumns() throws Exception {
        table.addColumn(new Column("ID", new BigDecimalType()));
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
    }

    public void addRows() throws Exception {
        {
            Row row = new SimpleRow(2);
            row.set(0, new Integer(1));
            row.set(1, "one");
            table.addRow(row);
        }
        {
            Row row = new SimpleRow(2);
            row.set(0, new Integer(2));
            row.set(1, "two");
            table.addRow(row);
        }
        assertEquals("Should have 2 rows", 2, table.getRowCount());
    }

    public void testAddRow() throws Exception {
        addColumns();
        addRows();
    }

    public void testTruncate() throws Exception {
        table.truncate();
        RowIterator iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertFalse(iter.hasNext());

        addColumns();
        addRows();
        iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertNotNull(iter.next());
        assertNotNull(iter.next());
        assertFalse(iter.hasNext());

        // create backup file before to test truncate deletes it
//        File df = getDataFile();
//        if (df != null) {
//            File bkupFile = new File(df.getParentFile(), df.getName() + ".backup");
//            FileOutputStream out = new FileOutputStream(bkupFile);
//            out.write("test".getBytes());
//            out.close();
//        }

        table.truncate();
        iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertFalse(iter.hasNext());

        addRows();
        iter = table.getRowIterator(true);
        assertNotNull(iter);
        assertNotNull(iter.next());
        assertNotNull(iter.next());
        assertFalse(iter.hasNext());
    }

    public void testDefrag() throws Exception {
//        Database db = getDatabase();
//
//        if (db instanceof DiskDatabase) {
//
//            DiskDatabase diskDB = (DiskDatabase) db;
//            addColumns();
//            addRows();
//            db.addTable(table);
//            table.shutdown();
//
//            long oldLength = FileUtil.getLength(getDataFile());
//            diskDB.defragTable(getTableName());
//            long newLength = FileUtil.getLength(getDataFile());
//            assertTrue("Expected " + oldLength + " = " + newLength, oldLength == newLength);
//
//            table = db.getTable(getTableName());
//            RowIterator iter = table.getRowIterator(false);
//            assertNotNull(iter);
//
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//            Row row = iter.current();
//            iter.remove();
//            table.addRow(new SimpleRow(row));
//            iter.reset();
//
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//            row = iter.current();
//            iter.set(new SimpleRow(row));
//
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//
//            assertFalse(iter.hasNext());
//
//            oldLength = FileUtil.getLength(getDataFile());;
//            diskDB.defragTable(getTableName());
//            
//            newLength = FileUtil.getLength(getDataFile());
//            
//            assertTrue("Expected " + oldLength + " > " + newLength, oldLength > newLength);
//
//            table = diskDB.getTable(getTableName());
//            iter = table.getRowIterator(false);
//            assertNotNull(iter);
//
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//
//            assertFalse(iter.hasNext());
//        }
    }

    public void testGetRowIterator() throws Exception {
//        table.addColumn(new Column("ID", new IntegerType()));
//        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
//        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
//        {
//            Row row = new SimpleRow(2);
//            row.set(0, new Integer(1));
//            row.set(1, "one");
//            table.addRow(row);
//        }
//        {
//            Row row = new SimpleRow(2);
//            row.set(0, new Integer(2));
//            row.set(1, "two");
//            table.addRow(row);
//        }
//        {
//            Row row = new SimpleRow(2);
//            row.set(0, new Integer(3));
//            row.set(1, "three");
//            table.addRow(row);
//        }
//        RowIterator iter = table.getRowIterator(false);
//        assertNotNull(iter);
//
//        try {
//            iter.current();
//            fail("Expected NoSuchElementException");
//        } catch (NoSuchElementException ex) {
//            // Expected
//        }
//
//        try {
//            iter.set(null);
//            fail("Expected IllegalStateException");
//        } catch (IllegalStateException ex) {
//            // Expected
//        }
//
//        try {
//            iter.remove();
//            fail("Expected IllegalStateException");
//        } catch (IllegalStateException ex) {
//            // Expected
//        }
//
//        // Iteration Pass 1
//        assertFalse(iter.hasPrevious());
//
//        try {
//            iter.previous();
//            fail("Expected NoSuchElementException");
//        } catch (NoSuchElementException ex) {
//            // Expected
//        }
//
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        assertNotNull(iter.current());
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        assertNotNull(iter.current());
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        assertNotNull(iter.current());
//
//        assertFalse(iter.hasNext());
//        assertEquals(iter.previousIndex(), iter.nextIndex() - 1);
//        try {
//            iter.next();
//            fail("Expected NoSuchElementException");
//        } catch (NoSuchElementException ex) {
//            // Expected
//        }
//
//        // Iteration Pass 2 : update row
//        Row row = new SimpleRow(2);
//        row.set(0, new Integer(4));
//        row.set(1, "newRow");
//
//        iter.reset();
//        assertFalse(iter.hasPrevious());
//
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        iter.set(row);
//        assertNotNull(iter.current());
//
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        iter.set(row);
//        assertNotNull(iter.current());
//
//        assertTrue(iter.hasNext());
//        assertNotNull(iter.next());
//        iter.set(row);
//        assertNotNull(iter.current());
//
//        assertFalse(iter.hasNext());
//
//        // Iteration Pass 3
//        assertTrue(iter.hasPrevious());
//        assertNotNull(iter.previous());
//        assertTrue(iter.hasPrevious());
//        assertNotNull(iter.previous());
//        assertTrue(iter.hasPrevious());
//        assertNotNull(iter.previous());
//
//        assertFalse(iter.hasPrevious());
//
//        assertTrue(iter.hasNext());
    }

    public void testNoNewColumnsAfterRowsAdded() throws Exception {
        table.addColumn(new Column("ID", new IntegerType()));
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
        Row row = new SimpleRow(2);
        row.set(0, new Integer(1));
        row.set(1, "one");
        table.addRow(row);
        try {
            table.addColumn(new Column("NAMETWO", new CharacterVaryingType(10)));
            fail("Expected AxionException");
        } catch (AxionException e) {
            // expected
        }
    }

    public void testGetColumnByIndex() throws Exception {
        table.addColumn(new Column("ID", new IntegerType()));
        assertEquals("ID", table.getColumn(0).getName());
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        assertEquals("ID", table.getColumn(0).getName());
        assertEquals("NAME", table.getColumn(1).getName());
    }

    public void testGetColumnByIndexBadIndex() throws Exception {
        try {
            table.getColumn(-1);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            table.getColumn(0);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        table.addColumn(new Column("ID", new IntegerType()));

        try {
            table.getColumn(-1);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            table.getColumn(1);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));

        try {
            table.getColumn(-1);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            table.getColumn(2);
            fail("Expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }
    }

    public void testGetAndFreeRowId() throws Exception {
//        int id = table.getNextRowId();
//        table.freeRowId(id);
//        assertEquals(id, table.getNextRowId());
//        int id2 = table.getNextRowId();
//        assertTrue(id != id2);
    }

    public void testGetColumnByName() throws Exception {
        table.addColumn(new Column("ID", new IntegerType()));
        assertTrue(table.getColumn("ID").getDataType() instanceof IntegerType);
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        assertTrue(table.getColumn("ID").getDataType() instanceof IntegerType);
        assertTrue(table.getColumn("NAME").getDataType() instanceof CharacterVaryingType);
    }

    public void testGetColumnByNameBadName() throws Exception {
        assertNull(table.getColumn("FOO"));
        assertNull(table.getColumn("ID"));
        table.addColumn(new Column("ID", new IntegerType()));
        assertNull(table.getColumn("FOO"));
        assertNull(table.getColumn("NAME"));
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        assertNull(table.getColumn("FOO"));
    }

    public void testDataTypes() throws Exception {
//        Table typeTable = createTable("TYPETABLE");
//
//        typeTable.addColumn(new Column("STRCOL", new CharacterVaryingType(30)));
//        typeTable.addColumn(new Column("INTCOL", new IntegerType()));
//        typeTable.addColumn(new Column("BOOLCOL", new BooleanType()));
//        ((SpreadsheetTable)typeTable).loadExternalTable(setProperties(typeTable.getName()));
//
//        Object[][] values = new Object[][] {
//                new Object[] { "", "A String", "Another String", null},
//                new Object[] { new Integer(17), new Integer(0), new Integer(5575), null},
//                new Object[] { Boolean.TRUE, Boolean.TRUE, Boolean.FALSE, null}};
//
//        Random random = new Random();
//        int numRows = 7;
//
//        for (int i = 0; i < numRows; i++) {
//            Row row = new SimpleRow(typeTable.getColumnCount());
//            for (int j = 0; j < typeTable.getColumnCount(); j++) {
//                row.set(j, values[j][random.nextInt(values[j].length)]);
//            }
//            typeTable.addRow(row);
//        }
//
//        RowIterator iter = typeTable.getRowIterator(true);
//        assertNotNull(iter);
//        for (int i = 0; i < numRows; i++) {
//            assertTrue(iter.hasNext());
//            assertNotNull(iter.next());
//        }
//        assertTrue(!iter.hasNext());
//        typeTable.shutdown();
    }

    public void testAddPrimaryKeyConstraintOnPopulatedTable() throws Exception {
        table.addColumn(new Column("ID", new BigDecimalType()));
        table.addColumn(new Column("NAME", new CharacterVaryingType(10)));
        ((SpreadsheetTable)table).loadExternalTable(setProperties(table.getName()));
        {
            Row row = new SimpleRow(2);
            row.set(0, new BigDecimal(1));
            row.set(1, "one");
            table.addRow(row);
        }
        {
            Row row = new SimpleRow(2);
            row.set(0, new BigDecimal(2));
            row.set(1, "two");
            table.addRow(row);
        }

        PrimaryKeyConstraint pk = new PrimaryKeyConstraint("PK_FOO");
        ColumnIdentifier colId = new ColumnIdentifier(new TableIdentifier(table.getName()), "ID");
        pk.addSelectable(colId);

        table.addConstraint(pk);

        // Adding duplicate ID should fail.
        {
            Row row = new SimpleRow(2);
            row.set(0, new BigDecimal(2));
            row.set(1, "two");
            try {
                table.addRow(row);
                fail("Expected AxionException on adding row with duplicate ID to table with PK");
            } catch (AxionException expected) {
                // Expected.
            }
        }

        // Now drop constraint, then add a duplicate row - primary key constraint should
        // fail on add
        table.removeConstraint("PK_FOO");
        {
            Row row = new SimpleRow(2);
            row.set(0, new BigDecimal(2));
            row.set(1, "two");
            table.addRow(row);
        }
        try {
            table.addConstraint(pk);
            fail("Expected AxionException on applying PK constraint to existing table with dup rows");
        } catch (AxionException expected) {
            // Expected.
        }
    }
}