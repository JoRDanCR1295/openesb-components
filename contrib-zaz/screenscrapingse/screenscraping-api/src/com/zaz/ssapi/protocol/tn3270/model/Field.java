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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.tn3270.model;

import com.zaz.ssapi.protocol.tn3270.util.AddressConvertUtil;


/**
 *
 * @author liyunhai
 */
public class Field {

    public static final String NORMAL_NOT_LIGHT = "00";
    public static final String NORMAL_LIGHT = "01";
    public static final String INTENSIFIED_LIGHT = "10";
    public static final String NOT_DISPLAYED = "11";
    private int startPos = 0;
    private int length = 0;
    private boolean fProtected = false;
    private boolean numeric = false;
    private boolean modifiedDataTag = false;
    private String displayStyled = "";
    private ExtField extField = new ExtField();
    private String recordNewValue = "";
    private String recordComment = "";
    private String recordParamName = "param";
    private boolean recordParamFlag = false;

    public String getDisplayStyled() {
        return displayStyled;
    }

    public void setDisplayStyled(String displayStyled) {
        this.displayStyled = displayStyled;
    }

    public boolean isFProtected() {
        return fProtected;
    }

    public void setFProtected(boolean fProtected) {
        this.fProtected = fProtected;
    }

    public int getLength() {
        return length;
    }

    public void setLength(int length) {
        this.length = length;
    }

    public boolean isModifiedDataTag() {
        return modifiedDataTag;
    }

    public void setModifiedDataTag(boolean modifiedDataTag) {
        this.modifiedDataTag = modifiedDataTag;
    }

    public boolean isNumeric() {
        return numeric;
    }

    public void setNumeric(boolean numeric) {
        this.numeric = numeric;
    }

    public int getStartPos() {
        return startPos;
    }

    public void setStartPos(int startPos) {
        this.startPos = startPos;
    }

    public ExtField getExtField() {
        return extField;
    }

    public void setExtField(ExtField extField) {
        this.extField = extField;
    }

    public String getRecordComment() {
        return recordComment;
    }

    public void setRecordComment(String recordComment) {
        this.recordComment = recordComment;
    }

    public String getRecordNewValue() {
        return recordNewValue;
    }

    public void setRecordNewValue(String recordNewValue) {
        this.recordNewValue = recordNewValue;
    }

    public boolean isRecordParamFlag() {
        return recordParamFlag;
    }

    public void setRecordParamFlag(boolean recordParamFlag) {
        this.recordParamFlag = recordParamFlag;
    }

    public String getRecordParamName() {
        return recordParamName;
    }

    public void setRecordParamName(String recordParamName) {
        this.recordParamName = recordParamName;
    }

    @Override
    public String toString() {
        return "startPos:" +
                AddressConvertUtil.getRowColAddressFromOffset(this.startPos) +
                " length:" + this.length + " endPos:" +
                AddressConvertUtil.getRowColAddressFromOffset((this.startPos + this.length - 1) % 1920) +
                " fProtected:" + this.isFProtected() + " numeric:" +
                this.isNumeric() + " displayStyled:" + this.getDisplayStyled() +
                " modifiedDataTag:" + this.isModifiedDataTag() + " extField:" +
                this.getExtField();
    }
}
