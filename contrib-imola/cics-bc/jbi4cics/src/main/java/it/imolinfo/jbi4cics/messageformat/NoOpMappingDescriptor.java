/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.messageformat;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.DynaProperty;
import org.apache.commons.beanutils.WrapDynaClass;
import org.apache.commons.collections.map.ListOrderedMap;

public class NoOpMappingDescriptor implements MappingDescriptor {
    
	/**
	 * The logger for this class and its instances.
	 */
	  private static final Logger LOG
	          = LoggerFactory.getLogger(NoOpMappingDescriptor.class);
	  
	/**
	 * The responsible to translate localized messages.
	 */
	  private static final Messages MESSAGES
	          = Messages.getMessages(NoOpMappingDescriptor.class);
	
  private Class beanClass;
  
  public NoOpMappingDescriptor() {
    super();
    // TODO Auto-generated constructor stub
  }

  public void addFieldMapping(String propertyName, String fieldName, FieldDescriptor fieldDescriptor) throws FormatException {
    throw new FormatException(MESSAGES.getString("CIC001600_Error_adding_field_mapping")); 
  }

  public void addFieldMapping(String propertyName, Integer fieldIndex, FieldDescriptor fieldDescriptor) throws FormatException {
    throw new FormatException(MESSAGES.getString("CIC001600_Error_adding_field_mapping")); 
  }

  public Map<String, FieldDescriptor> getFieldMap() throws FormatException {
    //TODO beanClass.
    WrapDynaClass dynaClass=WrapDynaClass.createDynaClass(beanClass);
    WrapDynaClass dynaSuperClass=WrapDynaClass.createDynaClass(beanClass.getSuperclass());
    DynaProperty[] superClassProperties=dynaSuperClass.getDynaProperties();
    DynaProperty[] properties=dynaClass.getDynaProperties();
    LOG.debug("class property: "+Arrays.toString(properties));
    LOG.debug("super class property: "+Arrays.toString(superClassProperties));
    List<DynaProperty> superClassPropertyList=Arrays.asList(superClassProperties);
    List<DynaProperty> classPropertyList=Arrays.asList(properties);

    List<DynaProperty> classPropertyListCopy=new ArrayList<DynaProperty>();
    for (Iterator<DynaProperty> i=classPropertyList.iterator();i.hasNext();){
      DynaProperty property=i.next();
      
      for (Iterator<DynaProperty> j=superClassPropertyList.iterator();j.hasNext();){
        DynaProperty superClassProperty=j.next();
        if (!(property.getName().equals(superClassProperty.getName()) && property.getType().equals(superClassProperty.getType()))){
          classPropertyListCopy.add(property);
        }
      }
    }
    LOG.debug("remained properties: "+classPropertyListCopy);
    Map<String, FieldDescriptor> fieldMap=new ListOrderedMap();
    for (Iterator<DynaProperty> i=classPropertyListCopy.iterator();i.hasNext();){
      final DynaProperty property=i.next();
      LOG.debug("adding property: "+property.getName());
      fieldMap.put(property.getName(),new FieldDescriptor(){
        public Class getPreferredJavaType(){
          return property.getType();
        }
      });
    }
    return fieldMap;
  }

  public void setBeanClass(Class beanClass) {
    this.beanClass=beanClass;
  }

  public Class getBeanClass() {
    return beanClass;
  }

  public void setCodePage(String codePage) {
    // TODO Auto-generated method stub
    
  }

}
