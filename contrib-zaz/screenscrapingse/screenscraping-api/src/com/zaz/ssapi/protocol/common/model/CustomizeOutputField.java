/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author liyunhai
 */
public class CustomizeOutputField implements Serializable {

    public static final int SINGLE_VALUE = 0;
    public static final int MULTI_VALUE = 1;
    public static final int RELATION_TABLE_VALUE = 2;
    public static final int CHILD_NODE_VALUE = 3;
    public static final int MAP_TABLE_VALUE = 4;
    public static final int LIST_TABLE_VALUE = 5;
    private int allScreenIndex = 0;
    private int outputType = 0;
    private int fieldType = 0;
    private String xsdName = null;
    private String keyword = null;
    private String separator = null;
    private String subSeparator = null;
    private int length = 0;
    private String tableHead = null;
    private int offset = 0;
    private int valueBeginRow = 0;
    private int valueEndRow = 0;
    private String treeNode = null;
    private List<RowColumnDef> rowCols = new ArrayList<RowColumnDef>();

    public void addRowCol(RowColumnDef rowCol) {
        rowCols.add(rowCol);
    }

    public void deleteRowCol(RowColumnDef rowCol) {
        if (rowCols.size() < 1) {
            return;
        }
        rowCols.remove(rowCol);
    }

    public int getOutputType() {
        return outputType;
    }

    public void setOutputType(int outputType) {
        this.outputType = outputType;
    }

    public int getAllScreenIndex() {
        return allScreenIndex;
    }

    public void setAllScreenIndex(int allScreenIndex) {
        this.allScreenIndex = allScreenIndex;
    }

    public String getXsdName() {
        return xsdName;
    }

    public void setXsdName(String xsdName) {
        this.xsdName = xsdName;
    }

    public int getFieldType() {
        return fieldType;
    }

    public void setFieldType(int fieldType) {
        this.fieldType = fieldType;
    }

    public List<RowColumnDef> getRowCols() {
        return rowCols;
    }

    public void setRowCols(List<RowColumnDef> rowCols) {
        this.rowCols = rowCols;
    }

    public String getKeyword() {
        return keyword;
    }

    public void setKeyword(String keyword) {
        this.keyword = keyword;
    }

    public int getLength() {
        return length;
    }

    public void setLength(int length) {
        this.length = length;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public int getValueBeginRow() {
        return valueBeginRow;
    }

    public void setValueBeginRow(int valueBeginRow) {
        this.valueBeginRow = valueBeginRow;
    }

    public int getValueEndRow() {
        return valueEndRow;
    }

    public void setValueEndRow(int valueEndRow) {
        this.valueEndRow = valueEndRow;
    }
    
    public String getSeparator() {
        return separator;
    }

    public void setSeparator(String separator) {
        this.separator = separator;
    }

    public String getSubSeparator() {
        return subSeparator;
    }

    public void setSubSeparator(String subSeparator) {
        this.subSeparator = subSeparator;
    }

    public String getTableHead() {
        return tableHead;
    }

    public void setTableHead(String tableHead) {
        this.tableHead = tableHead;
    }

    public String getTreeNode() {
        return treeNode;
    }

    public void setTreeNode(String treeNode) {
        this.treeNode = treeNode;
    }

    @Override
    public String toString() {
        if (SINGLE_VALUE == fieldType || MULTI_VALUE == fieldType) {
            return keyword;
        } else if (RELATION_TABLE_VALUE == fieldType || MAP_TABLE_VALUE == fieldType || LIST_TABLE_VALUE == fieldType) {
            return tableHead;
        } else {
            return treeNode;
        }
    }
}
