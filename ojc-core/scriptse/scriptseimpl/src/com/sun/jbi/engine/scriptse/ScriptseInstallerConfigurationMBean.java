/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.scriptse;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;


/**
 * ScriptSEInstallerConfigurationMBean.java Created on May 23, 2005, 1:49 PM
 *
 * @author blu
 */
public class ScriptseInstallerConfigurationMBean extends StandardMBean
    implements ScriptseConfigurationMBean {
    private String mSomeProperty = "someValue";

    /**
     * Creates a new ScriptseInstallerConfigurationMBean object.
     *
     * @throws NotCompliantMBeanException DOCUMENT ME!
     */
    public ScriptseInstallerConfigurationMBean() throws NotCompliantMBeanException {
        super(ScriptseConfigurationMBean.class);
    }

    /**
     * DOCUMENT ME!
     *
     * @param someProperty DOCUMENT ME!
     */
    public void setSomeProperty(String someProperty) {
        mSomeProperty = someProperty;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSomeProperty() {
        return mSomeProperty;
    }
}
