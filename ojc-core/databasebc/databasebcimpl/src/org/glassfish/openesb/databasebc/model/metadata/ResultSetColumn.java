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
 * @(#)ResultSetColumn.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.metadata;


/**
 * Class to hold resultset column metadata.
 *
 * @author Susan Chen
 * @version 
 */
public class ResultSetColumn {
    private String name = ""; // name of parameter
    private String javaType; // Java type - ex. java.lang.String
    private String sqlType; // SQL type - ex. BIGINT, NUMERIC
    private int ordinalPosition; // ordinal position
    private int numericPrecision; // numeric precision
    private int numericScale; // numeric scale
    private boolean isNullable; //specifies if the parameter is nullable
    private String label = ""; // title of column in resultset

    /**
     * Creates a new instance of ResultSetColumn.
     */
    public ResultSetColumn() {
        name = "";
        javaType = "";
        sqlType = "";
        ordinalPosition = 0;
        numericPrecision = 0;
        numericScale = 0;
        isNullable = false;
    }

    public ResultSetColumn(final ResultSetColumn rs) {
        name = rs.getName();
        javaType = rs.getJavaType();
        sqlType = rs.getSqlType();
        ordinalPosition = rs.getOrdinalPosition();
        numericPrecision = rs.getNumericPrecision();
        numericScale = rs.getNumericScale();
        isNullable = rs.getIsNullable();
        label = rs.getLabel();
    }

    /**
     * Creates a new instance of ResultSetColumn with the given name.
     *
     * @param newName ResultSetColumn name
     */
    public ResultSetColumn(final String newName) {
        name = newName;
    }

    /**
     * Creates a new instance of ResultSetColum with the given attributes.
     *
     * @param newName ResultSetColumn name
     * @param newJavaType Java type
     */
    public ResultSetColumn(final String newName, final String newJavaType) {
        name = newName;
        javaType = newJavaType;
    }

    /**
     * Creates a new instance of ResultSetColum with the given attributes.
     *
     * @param newName ResultSetColumn name
     * @param newJavaType Java type
     * @param newOrdinalPosition Ordinal position
     * @param newNumericPrecision Numeric precision
     * @param newNumericScale Numeric scale
     * @param newIsNullable Nullable flag
     */
    public ResultSetColumn(final String newName, final String newJavaType,
        final int newOrdinalPosition, final int newNumericPrecision, final int newNumericScale,
        final boolean newIsNullable) {
        name = newName;
        javaType = newJavaType;
        ordinalPosition = newOrdinalPosition;
        numericPrecision = newNumericPrecision;
        numericScale = newNumericScale;
        isNullable = newIsNullable;
    }

    /**
     * Get the ResultSet column name.
     *
     * @return ResultSet column name.
     */
    public String getName() {
        return name;
    }

    /**
     * Get the Java type.
     *
     * @return Java type
     */
    public String getJavaType() {
        return javaType;
    }

    /**
     * Get the SQL type.
     *
     * @return SQL type
     */
    public String getSqlType() {
        return sqlType;
    }

    /**
     * Get the ResultSet column ordinal position.
     *
     * @return ResultSet column ordinal position
     */
    public int getOrdinalPosition() {
        return ordinalPosition;
    }

    /**
     * Get the ResultSet column numeric precision.
     *
     * @return ResultSet column numeric precision
     */
    public int getNumericPrecision() {
        return numericPrecision;
    }

    /**
     * Get the ResultSet column numeric scale.
     *
     * @return ResultSet column numeric scale
     */
    public int getNumericScale() {
        return numericScale;
    }

    /**
     * Get the ResultSet column nullable flag.
     *
     * @return ResultSet column nullable flag.
     */
    public boolean getIsNullable() {
        return isNullable;
    }

    /**
     * Set the ResultSet column name.
     *
     * @param newName ResultSet column name
     */
    public void setName(final String newName) {
        name = newName;
    }

    /**
     * Set the ResultSet column Java type.
     *
     * @param newJavaType ResultSet column Java type.
     */
    public void setJavaType(final String newJavaType) {
        javaType = newJavaType;
    }

    /**
     * Set the ResultSet column SQL type.
     *
     * @param newSqlType ResultSet column SQL type.
     */
    public void setSqlType(final String newSqlType) {
        sqlType = newSqlType;
    }

    /**
     * Set the ResultSet column ordinal position.
     *
     * @param newOrdinalPosition ResultSet column ordinal position.
     */
    public void setOrdinalPosition(final int newOrdinalPosition) {
        ordinalPosition = newOrdinalPosition;
    }

    /**
     * Set the ResultSet column numeric precision.
     *
     * @param newNumericPrecision ResultSet column numeric precision.
     */
    public void setNumericPrecision(final int newNumericPrecision) {
        numericPrecision = newNumericPrecision;
    }

    /**
     * Set the ResultSet column numeric scale.
     *
     * @param newNumericScale ResultSet column numeric scale.
     */
    public void setNumericScale(final int newNumericScale) {
        numericScale = newNumericScale;
    }

    /**
     * Set the ResultSet column nullable flag.
     *
     * @param newIsNullable ResultSet column nullable flag
     */
    public void setIsNullable(final boolean newIsNullable) {
        isNullable = newIsNullable;
    }

    /**
     * Get the ResultSet column label.
     *
     * @return ResultSet column label.
     */
    public String getLabel() {
        return label;
    }

    /**
     * Set the ResultSet column label.
     *
     * @param newName ResultSet column label
     */
    public void setLabel(final String newName) {
        label = newName;
    }
}
