#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.test.framework;

import java.io.Serializable;

import junit.framework.TestCase;
import test.jbi.integration.testbc.core.Connection;
import test.jbi.integration.testbc.installer.TestSEInstaller;

import com.sun.jbi.ui.common.JBIRemoteException;

abstract public class IntegrationTestCase extends TestCase implements Serializable {

    transient private String mTestBC;
    transient private OpenESBInstaller mJbiInstaller;
    transient private Connection mCon;

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        mJbiInstaller = new OpenESBInstaller(Configuration.getJbiHost(), Configuration.getJbiPort(), Configuration.getJbiUsername(), Configuration.getJbiPassword(),
                Configuration.getJbiTarget());

        try {
            //make sure it is uninstalled
            mJbiInstaller.uninstallComponent("sun-test-engine");
        } catch (JBIRemoteException ex) {
        }

        String testBCDir = TestSEInstaller.generateInstaller(Configuration.getWorkingDir());

        mTestBC = mJbiInstaller.installComponent(testBCDir);
        mTestBC = mJbiInstaller.startComponent(mTestBC);

        mCon = new Connection("localhost", 9888);
        mCon.start();
    }

    @Override
    protected void tearDown() throws Exception {
        mCon.close();
        mJbiInstaller.uninstallComponent(mTestBC);
        super.tearDown();
    }

    protected OpenESBInstaller getInstaller() {
        return mJbiInstaller;
    }

    protected Connection getConnection() {
        return mCon;
    }
}
