package org.exolab.castor.xml.schema;

public interface SchemaEventListener extends java.util.EventListener {
    
    public void structureAdded(StructureEvent evt);
    
    public void structureRemoved(StructureEvent evt);

    public void attributeAdded(AttributeEvent evt);

    public void attributeRemoved(AttributeEvent evt);

    public void attributeModified(AttributeEvent evt);
}
