/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.model;

import java.io.Serializable;

/**
 *
 * @author tianlize
 */
public class RowColumnDef implements Serializable {

    private int columnBegin = 0;
    private int columnEnd = 0;
    private int valueLength = 0;
    private String xsdName = "";
    private String name = "";
    private String separator = "=";
    private int row = 0;

    public RowColumnDef(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {
        return this.getName().equals(((RowColumnDef) obj).getName());
    }

    public int getRow() {
        return row;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public String getXsdName() {
        return xsdName;
    }

    public void setXsdName(String xsdName) {
        this.xsdName = xsdName;
    }

    public String getSeparator() {
        return separator;
    }

    public void setSeparator(String separator) {
        this.separator = separator;
    }

    public int getValueLength() {
        return valueLength;
    }

    public void setValueLength(int valueLength) {
        this.valueLength = valueLength;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getColumnBegin() {
        return columnBegin;
    }

    public void setColumnBegin(int columnBegin) {
        this.columnBegin = columnBegin;
    }

    public int getColumnEnd() {
        return columnEnd;
    }

    public void setColumnEnd(int columnEnd) {
        this.columnEnd = columnEnd;
    }
}
