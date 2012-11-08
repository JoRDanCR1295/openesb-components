/*
 * $Id: TestRssTable.java,v 1.2 2008/10/14 10:32:48 jawed Exp $
 * =======================================================================
 * Copyright (c) 2002-2005 Axion Development Team.  All rights reserved.
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
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Statement;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.axiondb.io.FileUtil;
import org.axiondb.jdbc.AxionConnection;

/**
 *
 * @author RamaChandraiah
 */
public class TestRssTable extends TestCase {

    private AxionConnection _conn;
    private Statement _stmt;
    private String str = "create external table RSSTABLE (TITLE varchar2(1000),DESCRIPTION varchar2(2000)) " +
            "organization(loadtype='rss' url='http://rss.news.yahoo.com/rss/topstories');";
    private File _dbDir = new File(new File("."), "testdb6");
    private ResultSet _rset;

    public TestRssTable(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestRssTable.class);
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        _conn = (AxionConnection) (DriverManager.getConnection(getConnectString()));
        _stmt = _conn.createStatement();
    }

    @Override
    protected void tearDown() throws Exception {
        try {
            if (_stmt != null) {
                _stmt.close();
            }
        } catch (Exception t) {
        }
        _stmt = null;
        _conn = null;
        {
            Connection conn = DriverManager.getConnection(getConnectString());
            Statement stmt = conn.createStatement();
            stmt.execute("shutdown");
            stmt.close();
            conn.close();
        }
        deleteFile(getDatabaseDirectory());
    }

    protected boolean deleteFile(File file) throws Exception {
        return FileUtil.delete(file);
    }

    protected String getConnectString() {
        return "jdbc:axiondb:testdb:testdb6";
    }

    protected File getDatabaseDirectory() {
        return new File(new File("."), "testdb6");
    }

    public void testCreateTable() throws Exception {
        _stmt.execute(str);
        File tabledir = new File(_dbDir, "RSSTABLE");
        assertTrue("Table directory should exist", tabledir.exists());
    }

    public void testTableName() throws Exception {
        _stmt.execute(str);
        _rset = _stmt.executeQuery("SELECT * FROM RSSTABLE");
        ResultSetMetaData meta = _rset.getMetaData();
        assertEquals("File Name should be RSSTABLE", "RSSTABLE", meta.getTableName(1));

    }

    public void testDropTable() throws Exception {
        _stmt.execute(str);
        File tabledir = new File(_dbDir, "RSSTABLE");
        assertTrue("Table directory should exist", tabledir.exists());
        _stmt.execute("DROP TABLE RSSTABLE");
        assertTrue("Table directory should not exist", !(tabledir.exists()));
    }

    public void testColumnCount() throws Exception {
        _stmt.execute(str);
        ResultSet rset = _stmt.executeQuery("SELECT * FROM RSSTABLE");
        assertEquals("Should indicate 2 columns", 2, rset.getMetaData().getColumnCount());
    }
}
