/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

/**
 *
 * @author radval
 */
public class TesterInstallerConfigurationMBean extends StandardMBean implements TesterInstallerConfiguration {
    
    private String mSomeProperty = "someValue";
    
    public TesterInstallerConfigurationMBean() throws NotCompliantMBeanException  {
        super(TesterInstallerConfiguration.class);
    }

    public void setSomeProperty(String someValue) {
        mSomeProperty = someValue;
    }

    public String getSomeProperty() {
        return mSomeProperty;
    }
}
