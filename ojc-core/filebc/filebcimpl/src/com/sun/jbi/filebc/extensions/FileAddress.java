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
 * @(#)FileAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.filebc.Lock;

/**
 *
 * @author sweng
 * @author jfu
 */
public class FileAddress implements ExtensibilityElement, Serializable, Cloneable {

    private static final long serialVersionUID = 1L;
    private static final String USER_HOME_DIR = "User Home";
    private static final String TEMP_DIR = "Default System Temp Dir";
    private static final String USER_CURRENT_DIR = "Current Working Dir";
    QName fieldElementType = FileConstants.QNAME_ADDRESS;
    Boolean fieldRequired = null;
    Boolean relativePath = Boolean.FALSE;
    String fileDirectory;
    String pathRelativeTo;
    // added for polling recursively
    Boolean pollRecursive = Boolean.FALSE;
    String excludeRegex;
    String persistBaseLoc;
    String lockName = Lock.DEFAULT_INBOUND_LOCKFILE_NAME;
    String workArea = Lock.DEFAULT_INBOUND_TMPDIR_NAME;
    String seqName = Lock.DEFAULT_SEQFILE_NAME;

    public FileAddress() {
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    public void setRelativePath(Boolean val) {
        relativePath = val;
    }

    public Boolean getRelativePath() {
        return relativePath;
    }

    public String getFileDirectory() {
        return fileDirectory;
    }

    public void setFileDirectory(String val) {
        if (TEMP_DIR.equals(val.trim())) {
            fileDirectory = System.getProperty("java.io.tmpdir");
        } else {
            fileDirectory = val.trim();
        }
    }

    public String getPathRelativeTo() {
        return pathRelativeTo;
    }

    public String getLockName() {
        return lockName;
    }

    public void setLockName(String s) {
        lockName = s;
    }

    public String getWorkArea() {
        return workArea;
    }

    public void setWorkArea(String s) {
        workArea = s;
    }

    public String getSeqName() {
        return seqName;
    }

    public void setSeqName(String s) {
        seqName = s;
    }

    public String getPersistenceBaseLoc() {
        return persistBaseLoc;
    }

    public void setPersistenceBaseLoc(String s) {
        persistBaseLoc = s;
    }

    public String getExcludeRegex() {
        return excludeRegex;
    }

    public void setExcludeRegex(String s) {
        excludeRegex = s;
    }

    public Boolean getRecursive() {
        return pollRecursive;
    }

    public void setRecursive(Boolean val) {
        pollRecursive = val;
    }

    public void setPathRelativeTo(String val) {
        if (USER_HOME_DIR.equals(val)) {
            pathRelativeTo = System.getProperty("user.home");
        } else if (TEMP_DIR.equals(val)) {
            pathRelativeTo = System.getProperty("java.io.tmpdir");
        } else if (USER_CURRENT_DIR.equals(val)) {
            pathRelativeTo = System.getProperty("user.dir");
        } else {
            pathRelativeTo = System.getProperty("user.home");
        }
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFile address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nRelative Path=" + relativePath);
        strBuf.append("\nFile Directory=" + fileDirectory);
        strBuf.append("\nPath Relative To=" + pathRelativeTo);
        strBuf.append("\nLock File Name=" + lockName);
        strBuf.append("\nWork Area=" + workArea);
        strBuf.append("\nSequence File Name=" + seqName);
        strBuf.append("\nPoll or Fetch Recursively=" + pollRecursive != null ? pollRecursive.toString() : "NULL");
        strBuf.append("\nExclude When Poll or Fetch Recursively=" + excludeRegex != null ? excludeRegex : "NULL");
        strBuf.append("\nPersistence Base Loc=" + persistBaseLoc);

        return strBuf.toString();
    }

    @Override
    public FileAddress clone() throws CloneNotSupportedException {
        return (FileAddress) super.clone();
    }
}
