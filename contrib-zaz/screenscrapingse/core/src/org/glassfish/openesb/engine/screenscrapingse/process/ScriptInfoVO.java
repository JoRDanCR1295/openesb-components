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
 * @(#)ScriptInfoVO.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.process;

import org.glassfish.openesb.engine.screenscrapingse.scrconfig.Scriptconfig;


public class ScriptInfoVO {

    private Scriptconfig mScriptCfg = null;
    private String mScriptFileName = null;
    private String mEngineName = null;
    private WorkingMode mWorkMode = WorkingMode.CMPLX_MODE;

    /**
     * Creates a new instance of ScriptInfoVO
     */
    public ScriptInfoVO() {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Scriptconfig getMScriptCfg() {
        return mScriptCfg;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mScriptCfg DOCUMENT ME!
     */
    public void setMScriptCfg(Scriptconfig mScriptCfg) {
        this.mScriptCfg = mScriptCfg;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public WorkingMode getMWorkMode() {
        return mWorkMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public String getMEngineName() {
        return mEngineName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mEngineName
     */
    public void setMEngineName(String mEngineName) {
        this.mEngineName = mEngineName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mWorkMode
     */
    public void setMWorkMode(WorkingMode mWorkMode) {
        this.mWorkMode = mWorkMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public String getMScriptFileName() {
        return mScriptFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mScriptFileName
     */
    public void setMScriptFileName(String mScriptFileName) {
        this.mScriptFileName = mScriptFileName;
        //Setting the Script file, indicates that SE is working in a simple Mode
        // wherein the script is part of SU and only input parameters are passed to it.
        this.setMWorkMode(WorkingMode.SIMPLE_MODE);
    }

    /**
     *
     */
    public enum WorkingMode {

        CMPLX_MODE,
        SIMPLE_MODE;
    }
} //class ScriptInfoVO ends.
