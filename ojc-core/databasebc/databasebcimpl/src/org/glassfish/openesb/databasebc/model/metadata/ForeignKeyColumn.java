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
 * @(#)ForeignKeyColumn.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.metadata;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;


/**
 * Captures database foreign key metadata associated with a specific database
 * table column.
 *
 * @author Jonathan Giron
 * @version 
 */
public class ForeignKeyColumn extends KeyColumn {
    private static final String RS_PK_NAME = "PK_NAME"; // NOI18N
    private static final String RS_PKCATALOG_NAME = "PKTABLE_CAT"; // NOI18N
    private static final String RS_PKSCHEMA_NAME = "PKTABLE_SCHEM"; // NOI18N
    private static final String RS_PKTABLE_NAME = "PKTABLE_NAME"; // NOI18N
    private static final String RS_PKCOLUMN_NAME = "PKCOLUMN_NAME"; // NOI18N
    private static final String RS_FK_NAME = "FK_NAME"; // NOI18N
    private static final String RS_FKCOLUMN_NAME = "FKCOLUMN_NAME"; // NOI18N
    private static final String RS_UPDATE_RULE = "UPDATE_RULE"; // NOI18N
    private static final String RS_DELETE_RULE = "DELETE_RULE"; // NOI18N
    private static final String RS_DEFERRABILITY = "DEFERRABILITY"; // NOI18N
                                                                    /*
     * name of catalog containing foreign table whose primary key column is
     * associated with this foreign key
     */
    private static final Messages mMessages = Messages.getMessages(ForeignKeyColumn.class);
    private static final Logger mLogger = Messages.getLogger(ForeignKeyColumn.class);
    private String importCatalogName;

    /*
     * name of schema referencing foreign table whose primary key column is
     * associated with this foreign key
     */
    private String importSchemaName;

    /*
     * name of foreign table whose primary key column is associated with this
     * foreign key
     */
    private String importTableName;

    /* name of primary key column assocaited with this foreign key */
    private String importColumnName;

    /* name of import (primary) key associated with this foreign key */
    private String importKeyName;

    /* short flag indicating applicable update rule for this constraint */
    private short updateRule;

    /* short flag indicating applicable delete rule for this constraint */
    private short deleteRule;

    /* short flag indicating policy on evaluation of this constraint */
    private short deferrability;

    /**
     * Creates an instance of ForeignKeyColumn with the given values.
     *
     * @param fkName name of FK
     * @param fkColumn name of column assocaited with FK
     * @param pkName name of PK that this FK imports
     * @param pkColumn name of column that this FK imports
     * @param pkTable name of table containing column that this FK imports
     * @param pkSchema name of schema containing table with PK that this FK imports
     * @param pkCatalog name of catalog containing table with PK that this FK imports
     * @param colSequence sequence of this column within (composite) primary key
     * @param updateFlag applicable update rule for this FK; one of
     * java.sql.DatabaseMetaData.importedKeyNoAction,
     * java.sql.DatabaseMetaData.importedKeyCascade,
     * java.sql.DatabaseMetaData.importedKeySetNull,
     * java.sql.DatabaseMetaData#importedKeySetDefault, or
     * java.sql.DatabaseMetaData#importedKeyRestrict
     *
     * @param deleteFlag applicable delete rule for this FK; one of
     * java.sql.DatabaseMetaData.importedKeyNoAction,
     * java.sql.DatabaseMetaData.importedKeyCascade,
     * java.sql.DatabaseMetaData.importedKeySetNull,
     * java.sql.DatabaseMetaData.importedKeyRestrict, or
     * java.sql.DatabaseMetaData.importedKeySetDefault
     *
     * @param deferFlag deferrability flag for this FK; one of
     * java.sql.DatabaseMetaData.importedKeyInitiallyDeferred,
     * java.sql.DatabaseMetaData.importedKeyInitiallyImmediate, or
     * java.sql.DatabaseMetaData.importedKeyNotDeferrable
     *
     * @see java.sql.DatabaseMetaData#importedKeyCascade
     * @see java.sql.DatabaseMetaData#importedKeyInitiallyDeferred
     * @see java.sql.DatabaseMetaData#importedKeyInitiallyImmediate
     * @see java.sql.DatabaseMetaData#importedKeyNoAction
     * @see java.sql.DatabaseMetaData#importedKeyNotDeferrable
     * @see java.sql.DatabaseMetaData#importedKeyRestrict
     * @see java.sql.DatabaseMetaData#importedKeySetNull
     * @see java.sql.DatabaseMetaData#importedKeySetDefault
     */
    public ForeignKeyColumn(final String fkName, final String fkColumn, final String pkName,
        final String pkColumn, final String pkTable, final String pkSchema, final String pkCatalog,
        final short colSequence, final short updateFlag, final short deleteFlag, final short deferFlag) {
        super(fkName, fkColumn, colSequence);

        importKeyName = pkName;
        importCatalogName = pkCatalog;
        importSchemaName = pkSchema;
        importTableName = pkTable;
        importColumnName = pkColumn;

        setUpdateRule(updateFlag);
        setDeleteRule(deleteFlag);
        setDeferrability(deferFlag);
    }

    public ForeignKeyColumn(final ForeignKeyColumn fkCol) {
        super(fkCol.getName(), fkCol.getColumnName(), fkCol.getColumnSequence());

        importKeyName = fkCol.getImportKeyName();
        importCatalogName = fkCol.getImportCatalogName();
        importSchemaName = fkCol.getImportSchemaName();
        importTableName = fkCol.getImportTableName();
        importColumnName = fkCol.getImportColumnName();

        setUpdateRule(fkCol.getUpdateRule());
        setDeleteRule(fkCol.getDeleteRule());
        setDeferrability(fkCol.getDeferrability());
    }

    public ForeignKeyColumn(final ResultSet rs) throws SQLException {
        if (rs == null) {
            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n            
            throw new IllegalArgumentException(ForeignKeyColumn.mMessages.getString(
                    "DBBC_E00802.ERROR_VALID_RS") + "(ERROR_VALID_RS)");
        }

        importCatalogName = rs.getString(ForeignKeyColumn.RS_PKCATALOG_NAME);
        importSchemaName = rs.getString(ForeignKeyColumn.RS_PKSCHEMA_NAME);
        importTableName = rs.getString(ForeignKeyColumn.RS_PKTABLE_NAME);
        importColumnName = rs.getString(ForeignKeyColumn.RS_PKCOLUMN_NAME);
        importKeyName = rs.getString(ForeignKeyColumn.RS_PK_NAME);

        columnName = rs.getString(ForeignKeyColumn.RS_FKCOLUMN_NAME);
        keyName = rs.getString(ForeignKeyColumn.RS_FK_NAME);

        sequenceNum = rs.getShort(KeyColumn.RS_SEQUENCE_NUM);

        updateRule = rs.getShort(ForeignKeyColumn.RS_UPDATE_RULE);
        deleteRule = rs.getShort(ForeignKeyColumn.RS_DELETE_RULE);
        deferrability = rs.getShort(ForeignKeyColumn.RS_DEFERRABILITY);
    }

    /**
     * Creates a List of ForeignKeyColumn instances from the given ResultSet.
     *
     * @param rs ResultSet containing foreign key metadata as obtained from
     * DatabaseMetaData
     * @return List of ForeignKeyColumn instances based from metadata in rs
     *
     * @throws SQLException if SQL error occurs while reading in data from
     * given ResultSet
     */
    public static List<ForeignKeyColumn> createForeignKeyColumnList(final ResultSet rs)
        throws SQLException {
        if (rs == null) {
            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n
            throw new IllegalArgumentException(ForeignKeyColumn.mMessages.getString(
                    "DBBC_E00801.ERROR_NULL_RS") + "(ERROR_NULL_RS)");
        }

        List<ForeignKeyColumn> fkColumns = Collections.emptyList();

        if ((rs != null) && rs.next()) {
            fkColumns = new ArrayList<ForeignKeyColumn>();

            do {
                fkColumns.add(new ForeignKeyColumn(rs));
            } while (rs.next());
        }

        return fkColumns;
    }

    /**
     * Gets name of catalog containing the import table which, in turn,
     * contains the imported (primary) key associated with this foreign
     * key.
     *
     * @return name of catalog containing the imported primary key's
     * encapsulating table
     */
    private String getImportCatalogName() {
        return importCatalogName;
    }

    /**
     * Gets name of schema containing the import table which, in turn,
     * contains the imported (primary) key associated with this foreign
     * key.
     *
     * @return name of schema containing the imported primary key's
     * encapsulating table
     */
    private String getImportSchemaName() {
        return importSchemaName;
    }

    /**
     * Gets name of import table containing imported (primary) key
     * associated with this foreign key.
     *
     * @return name of table containing imported primary key
     */
    private String getImportTableName() {
        return importTableName;
    }

    /**
     * Gets name of import column contained within imported (primary) key
     * associated with this foreign key.
     *
     * @return name of imported column
     */
    private String getImportColumnName() {
        return importColumnName;
    }

    /**
     * Gets key name of imported (primary) key associated with this foreign
     * key.
     *
     * @return name of imported primary key
     */
    private String getImportKeyName() {
        return importKeyName;
    }

    /**
     * Gets update rule.
     *
     * @return update rule; one of
     * java.sql.DatabaseMetaData.importedKeyNoAction,
     * java.sql.DatabaseMetaData.importedKeyCascade,
     * java.sql.DatabaseMetaData.importedKeySetNull,
     * java.sql.DatabaseMetaData.importedKeyRestrict, or
     * java.sql.DatabaseMetaData.importedKeySetDefault.
     *
     * @see java.sql.DatabaseMetaData#importedKeyNoAction
     * @see java.sql.DatabaseMetaData#importedKeyCascade
     * @see java.sql.DatabaseMetaData#importedKeySetNull
     * @see java.sql.DatabaseMetaData#importedKeyRestrict
     * @see java.sql.DatabaseMetaData#importedKeySetDefault
     */
    private short getUpdateRule() {
        return updateRule;
    }

    /**
     * Gets delete rule.
     *
     * @return update rule; one of
     * java.sql.DatabaseMetaData.importedKeyNoAction,
     * java.sql.DatabaseMetaData.importedKeyCascade,
     * java.sql.DatabaseMetaData.importedKeySetNull,
     * java.sql.DatabaseMetaData.importedKeyRestrict, or
     * java.sql.DatabaseMetaData.importedKeySetDefault.
     *
     * @see java.sql.DatabaseMetaData#importedKeyNoAction
     * @see java.sql.DatabaseMetaData#importedKeyCascade
     * @see java.sql.DatabaseMetaData#importedKeySetNull
     * @see java.sql.DatabaseMetaData#importedKeyRestrict
     * @see java.sql.DatabaseMetaData#importedKeySetDefault
     */
    private short getDeleteRule() {
        return deleteRule;
    }

    /**
     * Gets deferrability flag.
     *
     * @return deferrability flag; one of
     * java.sql.DatabaseMetaData.importedKeyInitiallyDeferred,
     * java.sql.DatabaseMetaData.importedKeyInitiallyImmediate, or
     * java.sql.DatabaseMetaData.importedKeyNotDeferrable
     *
     * @see java.sql.DatabaseMetaData#importedKeyInitiallyDeferred,
     * @see java.sql.DatabaseMetaData#importedKeyInitiallyImmediate, or
     * @see java.sql.DatabaseMetaData#importedKeyNotDeferrable
     */
    private short getDeferrability() {
        return deferrability;
    }

    private void setUpdateRule(final short newRule) {
        switch (newRule) {
        case DatabaseMetaData.importedKeyNoAction:
        case DatabaseMetaData.importedKeyCascade:
        case DatabaseMetaData.importedKeySetNull:
        case DatabaseMetaData.importedKeySetDefault:
        case DatabaseMetaData.importedKeyRestrict:
            updateRule = newRule;

            break;

        default:

            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n                
            throw new IllegalArgumentException(ForeignKeyColumn.mMessages.getString(
                    "DBBC_E00803.ERROR_VALID_RULE") + "(ERROR_VALID_RULE)");
        }
    }

    private void setDeleteRule(final short newRule) {
        switch (newRule) {
        case DatabaseMetaData.importedKeyNoAction:
        case DatabaseMetaData.importedKeyCascade:
        case DatabaseMetaData.importedKeySetNull:
        case DatabaseMetaData.importedKeySetDefault:
        case DatabaseMetaData.importedKeyRestrict:
            deleteRule = newRule;

            break;

        default:

            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n                
            throw new IllegalArgumentException(ForeignKeyColumn.mMessages.getString(
                    "DBBC_E00803.ERROR_VALID_RULE") + "(ERROR_VALID_RULE)");
        }
    }

    private void setDeferrability(final short newFlag) {
        switch (newFlag) {
        case DatabaseMetaData.importedKeyInitiallyDeferred:
        case DatabaseMetaData.importedKeyInitiallyImmediate:
        case DatabaseMetaData.importedKeyNotDeferrable:
            deferrability = newFlag;

            break;

        default:
            System.err.println(
                "Received unrecognized value for newFlag, but carrying on with it anyway.");
            deferrability = newFlag;

            break;
        }
    }
}
