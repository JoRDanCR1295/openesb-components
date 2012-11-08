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
 * @(#)KeyColumn.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.model.metadata;

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
 *
 * @author Jonathan Giron
 * @version 
 */
public class KeyColumn {
    /**
     * DatabaseMetaData ResultSet column name used to decode name of associated
     * primary key
     */
    protected static final String RS_KEY_NAME = "PK_NAME"; // NOI18N
    private static final String RS_COLUMN_NAME = "COLUMN_NAME"; // NOI18N

    /**
     * DatabaseMetaData ResultSet column name used to decode key sequence number
     */
    protected static final String RS_SEQUENCE_NUM = "KEY_SEQ"; // NOI18N
    private static final Messages mMessages = Messages.getMessages(KeyColumn.class);
    private static final Logger mLogger = Messages.getLogger(KeyColumn.class);

    /** Name of column */
    protected String columnName;

    /** Name of key associated with this column */
    protected String keyName;

    /** For composite keys, sequence of this column for the associated key */
    protected int sequenceNum;

    /**
     * Creates an instance of KeyColumn with the given values.
     *
     * @param name name of key
     * @param column name of column
     * @param colSequence sequence of this column within (composite) primary key
     */
    public KeyColumn(final String name, final String column, final int colSequence) {
        keyName = name;
        columnName = column;
        sequenceNum = colSequence;
    }

    /** Creates a new instance of KeyColumn */
    protected KeyColumn() {
    }

    public KeyColumn(final ResultSet rs) throws SQLException {
        if (rs == null) {
            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n            
            throw new IllegalArgumentException(KeyColumn.mMessages.getString(
                    "SQLSE_E00802.ERROR_VALID_RS") + "(ERROR_VALID_RS)"); // NO i18n
        }

        keyName = rs.getString(KeyColumn.RS_KEY_NAME);
        columnName = rs.getString(KeyColumn.RS_COLUMN_NAME);
        sequenceNum = rs.getShort(KeyColumn.RS_SEQUENCE_NUM);
    }

    /**
     * Creates a List of (primary) KeyColumn instances from the given ResultSet.
     *
     * @param rs ResultSet containing primary key metadata as obtained from
     * DatabaseMetaData
     * @return List of KeyColumn instances based from metadata in rs
     *
     * @throws SQLException if SQL error occurs while reading in data from
     * given ResultSet
     */
    public static List<KeyColumn> createPrimaryKeyColumnList(final ResultSet rs)
        throws SQLException {
        if (rs == null) {
            final Locale locale = Locale.getDefault();
            final ResourceBundle cMessages = ResourceBundle.getBundle("com/stc/jdbc/builder/Bundle",
                    locale); // NO i18n            
            throw new IllegalArgumentException(cMessages.getString(
                    "SQLSE_E00801.ERROR_NULL_RS") + "(ERROR_NULL_RS)"); // NO i18n
        }

        List<KeyColumn> pkColumns = Collections.emptyList();

        if ((rs != null) && rs.next()) {
            pkColumns = new ArrayList<KeyColumn>();

            do {
                pkColumns.add(new KeyColumn(rs));
            } while (rs.next());
        }

        return pkColumns;
    }

    /**
     * Gets name of column name associate with this primary key.
     *
     * @return name of column
     */
    public String getColumnName() {
        return columnName;
    }

    /**
     * Gets name of primary key with which this column is associated.
     *
     * @return name of associated PK
     */
    public String getName() {
        return keyName;
    }

    /**
     * Gets sequence of this column within the (composite) primary key.
     *
     * @return column sequence
     */
    public int getColumnSequence() {
        return sequenceNum;
    }
}
