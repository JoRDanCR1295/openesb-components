package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;

public abstract class AbstractDbSpecial implements DbSpecial {

    private static final Messages mMessages = Messages.getMessages(AbstractDbSpecial.class);

    public int checkTableStatus(Connection con, String dbSchema, String tableName, Schema schema, Set columnNamesToIgnore) {
        PreparedStatement stmt = null;
        try {
            DatabaseMetaData dbmd = con.getMetaData();
            //by default we check table existence only in
            //schema as represent by username
            //this is true for both derby and oracle
            //where user name has a schema associated with them
            //and schema name matches user name
            if (dbSchema == null) {
                dbSchema = dbmd.getUserName();
            }

            // check if tableName already exists
            //note that here we are NOT using database meta data
            //api. those api are anyway slow and creates a deadlock
            //in a situation as described below:

            //We use schema and
            //table name to check existence of a table.
            //this is required because in operators like save stream
            //where this api is called, if iep is running on a database
            //and save stream is saving on same database as iep
            //then a when using database metadata getTable table scan without specifying schema and table
            //results in a deadlock which is because
            //iep deploy process logic obtains a lock on EMS_PLAN table, see Token
            //class and as part of that call save stream again tries to
            //scan tables using a different connection which includes this EMS_PLAN so a deadlock
            //occur since Token is released only at the end of deploy
            //which releases lock on EMS_PLAN and save stream table scan using
            //data base meta data api waits for EMS_PLAN to be available
            boolean nameExist = hasTable(con, dbSchema, tableName);
            if (!nameExist) {
                return TS_NAME_NOT_EXIST;
            }

            // check if two table have the same schema
            //prepare dummy query
            String identifierQuotedString = con.getMetaData().getIdentifierQuoteString();
            identifierQuotedString = identifierQuotedString.trim();
            StringBuffer sqlBuf = new StringBuffer("SELECT * FROM ");
            //for oracle/derby we always can safely use upper case
            //using lower case is not recognized as schema and table
            //name
            if (dbSchema != null) {
                sqlBuf.append(identifierQuotedString + convertToUpperCase(dbSchema) + identifierQuotedString + ".");
            }
            if (tableName != null) {
                sqlBuf.append(identifierQuotedString + convertToUpperCase(tableName) + identifierQuotedString);
            }
            sqlBuf.append(" WHERE 1 = 2");
            stmt = con.prepareStatement(sqlBuf.toString());
            stmt.executeQuery();

            //query is never executed on the server - only prepared
            ResultSetMetaData rsmd = stmt.getMetaData();
            int numcols = rsmd.getColumnCount();
            int columnCount = 0;
            for (int col = 0; col < numcols; col++) {
                String columnName = rsmd.getColumnName(col + 1);
                int type = rsmd.getColumnType(col + 1);

                boolean ignoreColumn = false;

                Iterator iterator = columnNamesToIgnore.iterator();
                while (iterator.hasNext()) {
                    String nameToIgnore = (String) iterator.next();
                    if (nameToIgnore.equalsIgnoreCase(columnName)) {
                        ignoreColumn = true;
                        break;
                    }
                }

                if (ignoreColumn) {
                    continue;
                }

                columnCount++;

                boolean foundName = false;
                ColumnMetadata clmd = null;

                for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
                    clmd = schema.getColumnMetadata(i);
                    if (clmd.getColumnName().equalsIgnoreCase(columnName)) {
                        foundName = true;
                        break;
                    }
                }
                if (!foundName) {
                    return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                }

                if (Util.getSqlType(clmd.getColumnType()) != type) {
                    return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                }

                if (clmd.hasColumnSize()) {
                    int columnSize = rsmd.getPrecision(col + 1);
                    if (clmd.getColumnSize() != columnSize) {
                        return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                    }
                    if (clmd.hasColumnScale()) {
                        int columnScale = rsmd.getScale(col + 1);
                        if (clmd.getColumnScale() != columnScale) {
                            return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                        }
                    }
                }
            }
            if (columnCount != schema.getColumnCount()) {
                return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
            }

            return TS_TABLE_EXIST;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "AbstractDbSpecial.Fail_to_check_status_of_table", tableName, e);
        } finally {
            Util.close(stmt);
        }
        return TS_UNKNOWN;
    }

    public boolean hasTable(Connection conn, String dbSchema, String tableName) throws Exception {
        PreparedStatement stmt = null;
        ResultSet results = null;
        try {
            String identifierQuotedString = conn.getMetaData().getIdentifierQuoteString();
            identifierQuotedString = identifierQuotedString.trim();
            StringBuffer sqlBuf = new StringBuffer("SELECT 'x' FROM ");
            if (dbSchema != null) {
                sqlBuf.append(identifierQuotedString + convertToUpperCase(dbSchema) + identifierQuotedString + ".");
            }
            if (tableName != null) {
                sqlBuf.append(identifierQuotedString + convertToUpperCase(tableName) + identifierQuotedString);
            }
            sqlBuf.append(" WHERE 1 = 2");
            stmt = conn.prepareStatement(sqlBuf.toString());
            results = stmt.executeQuery();
            return true;  // if table does exist, no rows will ever be returned
        } catch (SQLException e) {
            return false;  // if table does not exist, an exception will be thrown
        } finally {
            if (results != null) {
                results.close();
            }
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    //  for oracle/derby we always can safely use upper case
    //using lower case is not recognized as schema and table
    //name
    String convertToUpperCase(String name) {
        String language = System.getProperty("iepse.db.locale.language");
        String country = System.getProperty("iepse.db.locale.country");
        String variant = System.getProperty("iepse.db.locale.variant");

        Locale l = null;
        if (language != null && country != null && variant != null) {
            l = new Locale(language, country, variant);
        } else if (language != null && country != null) {
            l = new Locale(language, country);
        } else if (language != null) {
            l = new Locale(language);
        }

        if (l != null) {
            return name.toUpperCase(l);
        } else {
            return name.toUpperCase();
        }
    }
}
