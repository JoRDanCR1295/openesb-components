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
 * @(#)ResultSetColumns.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/*****************************************************************
 *
 * Copyright (c) 2002, SeeBeyond Technology Corporation,
 * All Rights Reserved
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * SEEBEYOND TECHNOLOGY CORPORATION.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ****************************************************************/

/**
 ** This class represents a parameter in a stored procedure which is a resultset.
 ** It is named ResultSetColumns to avoid confusing it with the jdbc resultset.
 **/
package com.sun.jbi.jdbcbc.model.metadata;

import java.util.ArrayList;


public class ResultSetColumns {
    private ArrayList<ResultSetColumn> columns = null;

    /**
     ** Holds the name of the ResultSet
     **/
    private String name = null;

    /**
     ** constructor
     **/
    public ResultSetColumns() {
        columns = new ArrayList<ResultSetColumn>();
    }

    /** getter for name;
     * @return name;
     */
    private String getName() {
        return name;
    }

    /** setter for name;
     ** @param name
     */
    private void setName(final String rsName) {
        name = rsName;
    }

    /** getter for numColumns;
     ** @return numColumns;
     **/
    private int getNumColumns() {
        return columns.size();
    }

    /** getter for columns;
     * @return columns;
     */
    private ArrayList getColumns() {
        return columns;
    }

    /** setter for columns;
     * @param columns list of <code>ResultSetColumn</code>
     ** objects;
     */
    private void setColumns(final ArrayList<ResultSetColumn> columns) {
        this.columns = columns;
    }

    /** adds a ResultsetColumn object to this list.
     * @param rsCol <code>ResultSetColumn</code>
     ** object that needs to be added;
     */
    private void add(final ResultSetColumn rsCol) {
        if (rsCol != null) {
            columns.add(rsCol);
        }
    }

    /** gets the ResultsetColumn object at the given index.
     * @param index index of <code>ResultSetColumn</code>
     ** object that needs to be retrieved;
     */
    private ResultSetColumn get(final int index) {
        return columns.get(index);
    }

    /** removes the given ResultSetColumn from the list
     * @param rsCol <code>ResultSetColumn</code>
     ** object that needs to be removed;
     * @returns true if the Object is in the list & is succesfully removed,
     * false otherwise.
     */
    private boolean remove(final ResultSetColumn rsCol) {
        Object removedRSCol = new Object();
        final int remIndex = columns.indexOf(rsCol);

        if (remIndex != -1) {
            removedRSCol = columns.remove(remIndex);
        }

        return removedRSCol.equals(rsCol);
    }

    /** removes a ResultSetColumn from the list at the given index
     * @param index index at which the
     ** object that needs to be removed was set;
     * @returns true if the Object is in the list & is succesfully removed,
     * false otherwise.
     */
    private boolean remove(final int index) {
        Object removedRSCol = null;
        removedRSCol = columns.remove(index);

        return (removedRSCol != null);
    }
}
