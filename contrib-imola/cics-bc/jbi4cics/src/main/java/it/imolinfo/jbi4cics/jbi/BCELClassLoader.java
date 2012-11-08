/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import java.util.HashMap;
import java.util.Map;

/**
 * A class loader implementation able to receive class definition that will be
 * found and defined on request.
 *
 * @author <a href="mailto:rspazzoli@imolinfo.it">Raffaele Spazzoli</a>
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class BCELClassLoader extends ClassLoader {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(BCELClassLoader.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(BCELClassLoader.class);

    /**
     * The bytecode of the classes added to (and loaded by) this
     * <code>ClassLoader</code>, indexed by their name.
     *
     * @see #addClass(String, byte[])
     */
    private Map<String, byte[]> classesCode = new HashMap<String, byte[]>();

    /**
     * Creates a new class loader using the specified parent class loader for
     * delegation.
     *
     * @param  parent  the parent class loader.
     */
    public BCELClassLoader(final ClassLoader parent) {
        super(parent);
    }

    /**
     * Adds a new class data to this class loader. After a call to this method,
     * this class loader is able to find and define the new specified class, if
     * parameters are valid.
     *
     * @param  className  the complete class name. Must be not
     *                    <code>null</code>.
     * @param  bytecode   the bytecode of the new class. Must be not
     *                    <code>null</code> and it is considered all its
     *                    content, starting from offset 0 and ending at offset
     *                    <code>bytecode.length</code> - 1. It is recommended
     *                    that the caller doesn't modify its content, because
     *                    this array is cached <i>as is</i>, without cloning it.
     */
    public void addClass(final String className, final byte[] bytecode) {
        classesCode.put(className, bytecode);
    }

    /**
     * Finds the class with the specified binary name, searching in the pool of
     * classes added by the {@link #addClass(String, byte[])} method.
     *
     * @param   name  the binary name of the class.
     * @return  the resulting <code>Class</code> object.
     * @throws  ClassNotFoundException  if the class could not be found, because
     *                                  it's not been added to this class
     *                                  loader.
     * @see     #addClass(String, byte[])
     */
    @Override
    protected Class<?> findClass(final String name)             // Overridden
            throws ClassNotFoundException {
        byte[] bytecode = classesCode.get(name);

        if (LOG.isDebugEnabled()) {
            LOG.debug("findClass(" + name + ")");
        }

        if (bytecode == null) {                         // Should never happen
            throw new ClassNotFoundException(
                    MESSAGES.getString("CIC001042_Class_not_found", name));
        }
        return defineClass(name, bytecode, 0, bytecode.length);
    }
}
