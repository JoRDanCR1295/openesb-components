/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.north.base;

import com.sun.nvs.core.netabs.devicedata.DataRecord;
import com.sun.nvs.core.netabs.devicedata.Obj2DataRecordIf;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface IOContext {
    //public CliViewData getViewData();
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getIOContextName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isInteractive();

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     */
    public void showError(String msg);

    /**
     * DOCUMENT ME!
     */
    public void clearErrorCount();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getErrorCount();

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     */
    public void showWarning(String msg);

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     */
    public void showMessage(String msg);

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     */
    public void showPaginatedMessage(String msg);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public boolean getConfirmation() throws Exception;

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean abortRequested();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public int readChar() throws Exception;

    /**
     * DOCUMENT ME!
     *
     * @param echoBack DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public String readLine(boolean echoBack) throws Exception;

    /**
     * DOCUMENT ME!
     */
    public void destroy();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ContextAttributeSet getAttributeSet();

    /**
     * DOCUMENT ME!
     *
     * @param record DOCUMENT ME!
     */
    public void addRecord(DataRecord record);

    /**
     * DOCUMENT ME!
     *
     * @param src DOCUMENT ME!
     */
    public void addRecordSource(Obj2DataRecordIf src);

    /**
     * DOCUMENT ME!
     *
     * @param record DOCUMENT ME!
     */
    public void addRecord(DataRecord[] record);

    /**
     * DOCUMENT ME!
     *
     * @param src DOCUMENT ME!
     */
    public void addRecordSource(Obj2DataRecordIf[] src);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isXmlContext();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getFormatType();
}
