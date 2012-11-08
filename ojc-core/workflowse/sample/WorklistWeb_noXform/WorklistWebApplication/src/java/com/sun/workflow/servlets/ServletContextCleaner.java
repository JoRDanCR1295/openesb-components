package com.sun.workflow.servlets;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

public class ServletContextCleaner implements ServletContextListener {
    private Class[] RELEASE_SIGNATURE = { ClassLoader.class };

    public void contextDestroyed(ServletContextEvent arg0) {
        // TODO Auto-generated method stub
        // Clean Up LogFactory, the code is taken from
        // org.apache.commons.logging.impl.ServletContextCleaner
        ClassLoader tccl = Thread.currentThread().getContextClassLoader();

        Object[] params = new Object[1];
        params[0] = tccl;

        // Walk up the tree of classloaders, finding all the available
        // LogFactory classes and releasing any objects associated with
        // the tccl (ie the webapp).
        //
        // When there is only one LogFactory in the classpath, and it
        // is within the webapp being undeployed then there is no problem;
        // garbage collection works fine.
        //
        // When there are multiple LogFactory classes in the classpath but
        // parent-first classloading is used everywhere, this loop is really
        // short. The first instance of LogFactory found will
        // be the highest in the classpath, and then no more will be found.
        // This is ok, as with this setup this will be the only LogFactory
        // holding any data associated with the tccl being released.
        //
        // When there are multiple LogFactory classes in the classpath and
        // child-first classloading is used in any classloader, then multiple
        // LogFactory instances may hold info about this TCCL; whenever the
        // webapp makes a call into a class loaded via an ancestor classloader
        // and that class calls LogFactory the tccl gets registered in
        // the LogFactory instance that is visible from the ancestor
        // classloader. However the concrete logging library it points
        // to is expected to have been loaded via the TCCL, so the
        // underlying logging lib is only initialised/configured once.
        // These references from ancestor LogFactory classes down to
        // TCCL classloaders are held via weak references and so should
        // be released but there are circumstances where they may not.
        // Walking up the classloader ancestry ladder releasing
        // the current tccl at each level tree, though, will definitely
        // clear any problem references.
        ClassLoader loader = tccl;

        while (loader != null) {
            // Load via the current loader. Note that if the class is not
            // accessable
            // via this loader, but is accessable via some ancestor then that
            // class
            // will be returned.
            try {
                Class logFactoryClass = loader
                        .loadClass("org.apache.commons.logging.LogFactory");
                Method releaseMethod = logFactoryClass.getMethod("release",
                        RELEASE_SIGNATURE);
                releaseMethod.invoke(null, params);
                loader = logFactoryClass.getClassLoader().getParent();

            } catch (ClassNotFoundException ex) {
                // Neither the current classloader nor any of its ancestors
                // could find
                // the LogFactory class, so we can stop now.
                loader = null;
            } catch (NoSuchMethodException ex) {
                // This is not expected; every version of JCL has this method
                System.err
                        .println("LogFactory instance found which does not support release method!");
                loader = null;
            } catch (IllegalAccessException ex) {
                // This is not expected; every ancestor class should be
                // accessable
                System.err
                        .println("LogFactory instance found which is not accessable!");
                loader = null;
            } catch (InvocationTargetException ex) {
                // This is not expected
                System.err
                        .println("LogFactory instance release method failed!");
                loader = null;
            }
        }

    }

    public void contextInitialized(ServletContextEvent arg0) {
        // TODO Auto-generated method stub

    }

}
