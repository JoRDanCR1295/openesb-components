/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.schema;

import java.util.List;
import javax.wsdl.Definition;
import javax.wsdl.extensions.schema.Schema;

/**
 * This Class provide the Definition of DefinitionAndSchema
 * 
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class DefinitionAndSchema {

    /**
     * The List of Definition.
     */
    private List<Definition> definitions = null;

    /**
     * The List of Schema for the Definitions
     */
    private List<Schema> schemas = null;

    /**
     * Flag that report if al-most one definition contains W3CEPR
     **/
    private boolean containsW3c = false;

    public DefinitionAndSchema() {
        containsW3c = false;
    }

    public DefinitionAndSchema(List<Definition> defList, List<Schema> schemList) {

        definitions = defList;
        schemas = schemList;
        containsW3c = false;
    }

    public void setDefinition(List<Definition> definitions) {

        this.definitions = definitions;
    }

    /**
     * Return the List of Definition
     * 
     * @return list of Definition
     */
    public List<Definition> getDefinitions() {
        return definitions;
    }

    public void setSchemas(List<Schema> schemas) {
        this.schemas = schemas;

    }

    /**
     * Return the List of Definition
     * 
     * @return list of Schema
     */
    public List<Schema> getSchemas() {
        return schemas;
    }

    public boolean isContainsW3c() {
        return containsW3c;
    }

    public void setContainsW3c(boolean containsW3c) {
        this.containsW3c = containsW3c;
    }

}
