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
 * @(#)IndexColumn.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.metadata;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;


/**
 * Captures database index metadata associated with a specific database table
 * column.
 *
 * @author Jonathan Giron
 * @version 
 */
public class IndexColumn implements Comparable {
    private static final String RS_INDEX_NAME = "INDEX_NAME"; // NOI18N
    private static final String RS_COLUMN_NAME = "COLUMN_NAME"; // NOI18N
    private static final String RS_NON_UNIQUE = "NON_UNIQUE"; // NOI18N
    private static final String RS_TYPE = "TYPE"; // NOI18N
    private static final String RS_ORDINAL = "ORDINAL_POSITION"; // NOI18N
    private static final String RS_ASC_OR_DESC = "ASC_OR_DESC"; // NOI18N
    private static final String RS_CARDINALITY = "CARDINALITY"; // NOI18N
    private static final Messages mMessages = Messages.getMessages(IndexColumn.class);
    private static final Logger mLogger = Messages.getLogger(IndexColumn.class);

    /* Name of index */
    private String name;

    /* Name of column associated with this index */
    private String columnName;

    /* Index type: tableIndexClustered, tableIndexHashed, tableIndexOther */
    private int type;

    /* Column sequence number within a composite index */
    private int ordinalPosition;

    /* Indicates whether index is unique */
    private boolean unique = false;

    /* Indicates sort order, if any, of index */
    private String sortSequence;

    /* Indicates number of unique values in index */
    private int cardinality;

    public IndexColumn(final ResultSet rs) throws SQLException {
        if (rs == null) {
            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n            
            throw new IllegalArgumentException(IndexColumn.mMessages.getString(
                    "ERROR_VALID_RS") + "(ERROR_VALID_RS)"); // NOI18N
        }

        name = rs.getString(IndexColumn.RS_INDEX_NAME);
        columnName = rs.getString(IndexColumn.RS_COLUMN_NAME);

        unique = !(rs.getBoolean(IndexColumn.RS_NON_UNIQUE));
        type = rs.getShort(IndexColumn.RS_TYPE);

        ordinalPosition = rs.getShort(IndexColumn.RS_ORDINAL);
        sortSequence = rs.getString(IndexColumn.RS_ASC_OR_DESC);
        cardinality = rs.getInt(IndexColumn.RS_CARDINALITY);
    }

    /**
     * Creates a new instance of IndexColumn using the given values.
     *
     * @param colName name of column
     * @param indexName name of index
     * @param indexType index type, as defined in DatabaseMetaData; note that
     * tableIndexStatistic is not a valid type for IndexColumn
     * @param colOrder sequence of this column within the (composite) index;
     * should be 1 for single-column indexes
     * @param isUnique true if index enforces unique values, false otherwise
     * @param sortOrder "A" for ascending order, "D" for descending order, null for
     * no defined sort order
     * @param indexCardinality count of unique values contained in the index
     */
    public IndexColumn(final String colName, final String indexName, final short indexType,
        final short colOrder, final boolean isUnique, final String sortOrder, final int indexCardinality) {
        final Locale locale = Locale.getDefault();
        final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                locale); // NO i18n            

        if (colName == null) {
            throw new IllegalArgumentException(IndexColumn.mMessages.getString(
                    "DBBC_E00805.ERROR_PARAMETER_NAME") + "(ERROR_PARAMETER_NAME)"); // NOI18N
        }

        if ((indexType != DatabaseMetaData.tableIndexClustered) &&
                (indexType != DatabaseMetaData.tableIndexHashed) &&
                (indexType != DatabaseMetaData.tableIndexOther)) {
            throw new IllegalArgumentException(MessageFormat.format(
                    IndexColumn.mMessages.getString("DBBC_E00804.ERROR_INDEX_TYPE"),
                    new Object[] { new Integer(type) }) + "(ERROR_INDEX_TYPE)"); // NOI18N
        }

        name = indexName;
        columnName = colName;

        type = indexType;
        ordinalPosition = colOrder;
        unique = isUnique;

        sortSequence = sortOrder;
        cardinality = indexCardinality;
    }

    public IndexColumn(final IndexColumn iCol) {
        final Locale locale = Locale.getDefault();
        final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                locale); // NO i18n        

        if (iCol.getColumnName() == null) {
            throw new IllegalArgumentException(IndexColumn.mMessages.getString(
                    "ERROR_PARAMETER_NAME") + "(ERROR_PARAMETER_NAME)"); // NOI18N
        }

        if ((iCol.getType() != DatabaseMetaData.tableIndexClustered) &&
                (iCol.getType() != DatabaseMetaData.tableIndexHashed) &&
                (iCol.getType() != DatabaseMetaData.tableIndexOther)) {
            throw new IllegalArgumentException(MessageFormat.format(
                    IndexColumn.mMessages.getString("DBBC_E00804.ERROR_INDEX_TYPE"),
                    new Object[] { new Integer(type) }) + "(ERROR_INDEX_TYPE)"); // NOI18N
        }

        name = iCol.getIndexName();
        columnName = iCol.getColumnName();

        type = iCol.getType();
        ordinalPosition = iCol.getOrdinalPosition();
        unique = iCol.isUnique();

        sortSequence = iCol.getSortOrder();
        cardinality = iCol.getCardinality();
    }

    private IndexColumn() {
    }

    /**
     * Creates a List of IndexColumn instances from the given ResultSet.
     *
     * @param rs ResultSet containing index metadata as obtained from
     * DatabaseMetaData
     * @return List of IndexColumn instances based from metadata in rs'
     *
     * @throws SQLException if SQL error occurs while reading in data from
     * given ResultSet
     */
    public static List<IndexColumn> createIndexList(final ResultSet rs) throws SQLException {
        List<IndexColumn> indices = Collections.emptyList();

        if ((rs != null) && rs.next()) {
            indices = new ArrayList<IndexColumn>();

            do {
                final IndexColumn newIndex = new IndexColumn(rs);

                // Ignore statistics indexes as they are relevant only to the
                // DB which sourced this metadata.
                if (newIndex.getType() != DatabaseMetaData.tableIndexStatistic) {
                    indices.add(newIndex);
                }
            } while (rs.next());
        }

        return indices;
    }

    /**
     * Gets index name.
     *
     * @return index name
     */
    public String getIndexName() {
        return name;
    }

    /**
     * Gets column name.
     *
     * @return name of column
     */
    public String getColumnName() {
        return columnName;
    }

    /**
     * Gets index type.
     *
     * @return index type, one of DatabaseMetaData.tableIndexClustered,
     * DatabaseMetaData.tableIndexHashed, or DatabaseMetaData.tableIndexOther
     *
     * @see java.sql.DatabaseMetaData.tableIndexClustered
     * @see java.sql.DatabaseMetaData.tableIndexHashed
     * @see java.sql.DatabaseMetaData.tableIndexOther
     */
    public int getType() {
        return type;
    }

    /**
     * Indicates whether the index associated with this column is unique.
     *
     * @return true if index is unique, false otherwise
     */
    public boolean isUnique() {
        return unique;
    }

    /**
     * Gets ordinal position of this column within a (composite) index.
     * For an index associated with a single column, this should be 1.
     *
     * @return ordinal position of column within the composite index; 1 if
     */
    public int getOrdinalPosition() {
        return ordinalPosition;
    }

    /**
     * Gets sort order, if any, associated with this index.
     *
     * @return "A" for ascending, "D" for descending, or null if no sort order
     * is defined
     */
    public String getSortOrder() {
        return sortSequence;
    }

    /**
     * Gets index cardinality.  It represents the number of unique
     * values associated with the index (for clustered, hashed, or other
     * index types).
     *
     * @return count of unique values in the associated index
     */
    public int getCardinality() {
        return cardinality;
    }

    /**
     * Compares this instance against the given object with respect to some
     * "natural" ordering.  Useful for sorting IndexColumn instances for UI display.
     *
     * Note: this class has a natural ordering that is inconsistent with equals.
     * @param o Object against which this will be compared
     * @return -1 if this is lesser than o; 0 if this is equal to o; 1 if this
     * is greater than o
     */
    //@Override
    public int compareTo(final Object o) {
        if (o == this) {
            return 0;
        }

        final IndexColumn idx = (IndexColumn) o;

        if (idx != null) {
            if (name != null) {
                return (idx.name != null) ? name.compareTo(idx.name) : (-1);
            } else if (columnName != null) {
                return (idx.columnName != null)
                ? columnName.compareTo(idx.columnName) : (-1);
            } else {
                return 0;
            }
        } else {
            return -1;
        }
    }
}
