/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Table.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.metadata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;


/**
 * Class to hold procedure metadata.
 *
 * @author
 */
public class Table {
    private static final Messages mMessages = Messages.getMessages(Table.class);
    private static final Logger mLogger = Messages.getLogger(Table.class);
    private String name = ""; // name of table
    private String javaName = ""; // java name of table
    private String catalog = ""; // catalog
    private String schema = ""; // schema
    private int numColumns = 0; // number of table columns
    private int numColumnsSelected = 0; // number of table columns selected
    private TableColumn[] columns; // array of table columns
    private String type = "TABLE"; // TABLE, SYSTEM TABLE, VIEW - from driver
    private List<IndexColumn> indexList; // List of IndexColumn objects
    private List<ForeignKeyColumn> fkColumnList; // List of KeyColumn objects (PK cols)
    private List<KeyColumn> pkColumnList; // List of ForeignKeyColumn objects (FK cols)
    private boolean selected;

    /**
     * Creates an instance of Table with the given attributes.
     *
     * @param tname Table name
     * @param tcatalog Catalog name
     * @param tschema Schema name
     * @param ttype Table type
     */
    public Table(final String tname, final String tcatalog, final String tschema, final String ttype) {
        name = tname;
        catalog = tcatalog;
        schema = tschema;
        type = ttype;

        indexList = Collections.emptyList();
        fkColumnList = Collections.emptyList();
        pkColumnList = Collections.emptyList();
    }

    /**
     * Creates an instance of Table with the given attributes.
     *
     * @param tname Table name
     * @param jname Table java name
     * @param tcatalog Catalog name
     * @param tschema Schema name
     * @param ttype Table type
     */
    public Table(final String tname, final String jname, final String tcatalog, final String tschema,
        final String ttype) {
        name = tname;
        javaName = jname;
        catalog = tcatalog;
        schema = tschema;
        type = ttype;

        indexList = Collections.emptyList();
        fkColumnList = Collections.emptyList();
        pkColumnList = Collections.emptyList();
    }

    /**
     * Creates an instance of Table with the given attributes.
     *
     * @param nTable 
     */
    public Table(final Table nTable) {
        name = nTable.getName();
        javaName = nTable.getJavaName();
        catalog = nTable.getCatalog();
        schema = nTable.getSchema();
        numColumns = nTable.getNumColumns();
        numColumnsSelected = nTable.getNumColumnsSelected();
        cloneColumns(nTable.getColumns());
        type = nTable.getType();
        cloneIndexList(nTable.getIndexList());
        cloneForeignKeyColumnList(nTable.getForeignKeyColumnList());
        clonePrimaryKeyColumnList(nTable.getPrimaryKeyColumnList());
        selected = nTable.isSelected();
    }

    /**
     * Get the table name.
     *
     * @return Table name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the table java name.
     *
     * @return Table java name
     */
    public String getJavaName() {
        return javaName;
    }

    /**
     * Get the catalog name.
     *
     * @return Catalog name
     */
    public String getCatalog() {
        return catalog;
    }

    /**
     * Get the schema name.
     *
     * @return Schema name
     */
    public String getSchema() {
        return schema;
    }

    /**
     * Get the number of table columns.
     *
     * @return Number of table columns.
     */
    public int getNumColumns() {
        return numColumns;
    }

    /**
     * Get the number of columns selected.
     *
     * @return Number of columns selected.
     */
    public int getNumColumnsSelected() {
        return numColumnsSelected;
    }

    /**
     * Get the list of table columns.
     *
     * @return List of table columns
     */
    public TableColumn[] getColumns() {
        return columns;
    }

    /**
     * Get the table type.
     *
     * @return Table type
     */
    public String getType() {
        return type;
    }

    /**
     * Set the table name.
     *
     * @param newName Table name
     */
    public void setName(final String newName) {
        name = newName;
    }

    /**
     * Set the table java name.
     *
     * 
     * @param newJavaName 
     */
    public void setJavaName(final String newJavaName) {
        javaName = newJavaName;
    }

    /**
     * Set the catalog name.
     *
     * @param newCatalog Catalog name
     */
    public void setCatalog(final String newCatalog) {
        catalog = newCatalog;
    }

    /**
     * Set the schema name.
     *
     * @param newSchema Schema name
     */
    public void setSchema(final String newSchema) {
        schema = newSchema;
    }

    /**
     * Set the table columns.
     *
     * @param newColumns Table columns
     */
    public void setColumns(final TableColumn[] newColumns) {
        columns = newColumns;

        // update the number of columns and columns selected
        if (columns != null) {
            numColumns = columns.length;

            int count = 0;

            for (TableColumn element : columns) {
                if (element.getIsSelected()) {
                    count++;
                }
            }

            numColumnsSelected = count;
        } else {
            numColumns = 0;
            numColumnsSelected = 0;
        }
    }

    /**
     * Clone the table columns.
     *
     * @param newColumns Table columns
     */
    public void cloneColumns(final TableColumn[] newColumns) {
        numColumns = 0;
        numColumnsSelected = 0;

        int count = 0;

        if (newColumns != null) {
            numColumns = newColumns.length;

            if (numColumns > 0) {
                columns = new TableColumn[numColumns];

                for (int i = 0; i < numColumns; i++) {
                    columns[i] = new TableColumn(newColumns[i]);

                    if (columns[i].getIsSelected()) {
                        count++;
                    }
                }
            }

            numColumnsSelected = count;
        }
    }

    public void addColumn(final TableColumn col) {
        if (null == col) {
            return;
        }

        int numCols = 0;

        if (null != columns) {
            numCols = columns.length;
        }

        final TableColumn[] newTable = new TableColumn[numCols + 1];

        for (int i = 0; i < numCols; i++) {
            newTable[i] = columns[i];
        }

        newTable[numCols] = col;
        setColumns(newTable);
    }

    public void removeColumn(final int index) {
        if ((null == columns) || (index > columns.length)) {
            return;
        }

        final int numCols = columns.length;

        final TableColumn[] newTable = new TableColumn[numCols - 1];

        for (int i = 0, j = 0; i < numCols; i++, j++) {
            if (i == index) {
                j--;
            } else {
                newTable[j] = columns[i];
            }
        }

        setColumns(newTable);
    }

    /**
     * Set the table type.
     *
     * @param newType Table type
     */
    public void setType(final String newType) {
        type = newType;
    }

    /**
     * Get the index list.
     *
     * @return Index list
     */
    public List<IndexColumn> getIndexList() {
        return indexList;
    }

    /**
     * Set the index list.
     *
     * @param newList Index list
     */
    public void setIndexList(final List<IndexColumn> newList) {
        if ((newList != null) && (newList.size() != 0)) {
            try {
                // Test to ensure that List contains nothing but Index objects.
                final IndexColumn[] dummy = newList.toArray(new IndexColumn[newList.size()]);
            } catch (final ArrayStoreException e) {
                throw new IllegalArgumentException(
                    "newList does not contain Index objects!");
            }

            indexList = newList;
        }
    }

    public void cloneIndexList(final List<IndexColumn> newList) {
        indexList = Collections.emptyList();

        if ((newList != null) && (newList.size() != 0)) {
            indexList = new ArrayList<IndexColumn>();

            try {
                // Test to ensure that List contains nothing but Index objects.
                final IndexColumn[] dummy = newList.toArray(new IndexColumn[newList.size()]);

                for (int i = 0; i < newList.size(); i++) {
                    final IndexColumn iCol = newList.get(i);
                    indexList.add(new IndexColumn(iCol));
                }
            } catch (final ArrayStoreException e) {
                mLogger.log(Level.SEVERE,"newList does not contain Index objects!",e);
                throw new IllegalArgumentException(
                    "newList does not contain Index objects!");
            }
        }
    }

    //added by Neena
    //to set the selection state of the table 
    public void setSelected(final boolean selected) {
        this.selected = selected;
    }

    //added by Neena
    // to get the selection state of the object
    public boolean isSelected() {
        return selected;
    }

    /**
     * Gets current List of KeyColumn objects, representing primary key columns
     * in this table.
     *
     * @return List (possibly empty) of KeyColumn instances
     */
    public List<KeyColumn> getPrimaryKeyColumnList() {
        return pkColumnList;
    }

    /**
     * Sets List of primary key column objects to the given List.
     *
     * @param newList List containing new collection of KeyColumn objects
     * representing primary key columns within this table
     */
    public void setPrimaryKeyColumnList(final List<KeyColumn> newList) {
        if ((newList != null) && (newList.size() != 0)) {
            try {
                // Test to ensure that List contains nothing but Index objects.
                final KeyColumn[] dummy = newList.toArray(new KeyColumn[newList.size()]);
            } catch (final ArrayStoreException e) {
                //  ResourceBundle cMessages = NbBundle.getBundle(Table.class);
                mLogger.log(Level.SEVERE,mMessages.getString("DBBC_E00806.ERROR_KEY"),e);
                throw new IllegalArgumentException(Table.mMessages.getString(
                        "DBBC_E00806.ERROR_KEY")); // NO i18n
            }

            pkColumnList = newList;
        }
    }

    public void clonePrimaryKeyColumnList(final List<KeyColumn> newList) {
        pkColumnList = Collections.emptyList();

        if ((newList != null) && (newList.size() != 0)) {
            pkColumnList = new ArrayList<KeyColumn>();

            try {
                // Test to ensure that List contains nothing but Index objects.
                final KeyColumn[] dummy = newList.toArray(new KeyColumn[newList.size()]);

                for (int i = 0; i < newList.size(); i++) {
                    final KeyColumn tPK = newList.get(i);
                    pkColumnList.add(new KeyColumn(tPK.getName(),
                            tPK.getColumnName(), tPK.getColumnSequence()));
                }
            } catch (final ArrayStoreException e) {
                // ResourceBundle cMessages = NbBundle.getBundle(Table.class);
                mLogger.log(Level.SEVERE,mMessages.getString("DBBC_E00806.ERROR_KEY"),e);
                throw new IllegalArgumentException(Table.mMessages.getString(
                        "DBBC_E00806.ERROR_KEY")); // NO i18n
            }
        }
    }

    /**
     * Gets current List of ForeignKeyColumn objects, representing foreign key
     * columns in this table.
     *
     * @return List (possibly empty) of ForeignKeyColumn instances
     */
    private List<ForeignKeyColumn> getForeignKeyColumnList() {
        return fkColumnList;
    }

    /**
     * Sets List of foreign key column objects to the given List.
     *
     * @param newList List containing new collection of ForeignKeyColumn objects
     * representing foreign key columns within this table
     */
    public void setForeignKeyColumnList(final List<ForeignKeyColumn> newList) {
        if ((newList != null) && (newList.size() != 0)) {
            try {
                // Test to ensure that List contains nothing but Index objects.
                final ForeignKeyColumn[] dummy = newList.toArray(new ForeignKeyColumn[newList.size()]);
            } catch (final ArrayStoreException e) {
                //  ResourceBundle cMessages = NbBundle.getBundle(Table.class);
                //throw new IllegalArgumentException(
                //        cMessages.getString("ERROR_FK_KEY")+"(ERROR_FK_KEY)");// NO i18n
                mLogger.log(Level.SEVERE,mMessages.getString("DBBC_E00806.ERROR_KEY"),e);
                throw new IllegalArgumentException(Table.mMessages.getString(
                        "DBBC_E00806.ERROR_KEY")); // NO i18n
            }

            fkColumnList = newList;
        }
    }

    private void cloneForeignKeyColumnList(final List<ForeignKeyColumn> newList) {
        fkColumnList = Collections.emptyList();

        if ((newList != null) && (newList.size() != 0)) {
            fkColumnList = new ArrayList<ForeignKeyColumn>();

            try {
                // Test to ensure that List contains nothing but Index objects.
                final ForeignKeyColumn[] dummy = newList.toArray(new ForeignKeyColumn[newList.size()]);

                for (int i = 0; i < newList.size(); i++) {
                    final ForeignKeyColumn fkCol = newList.get(i);
                    fkColumnList.add(new ForeignKeyColumn(fkCol));
                }
            } catch (final ArrayStoreException e) {
                //     ResourceBundle cMessages = NbBundle.getBundle(Table.class);
                //     throw new IllegalArgumentException(
                //             cMessages.getString("ERROR_FK_KEY")+"(ERROR_FK_KEY)");// NO i18n
                mLogger.log(Level.SEVERE,mMessages.getString("DBBC_E00806.ERROR_KEY"),e);
                throw new IllegalArgumentException(Table.mMessages.getString(
                        "DBBC_E00806.ERROR_KEY")); // NO i18n
            }
        }
    }
}
