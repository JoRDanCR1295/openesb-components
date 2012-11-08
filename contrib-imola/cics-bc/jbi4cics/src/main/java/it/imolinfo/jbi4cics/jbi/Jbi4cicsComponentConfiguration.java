/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import org.apache.servicemix.common.PersistentConfiguration;

public class Jbi4cicsComponentConfiguration extends PersistentConfiguration
    implements Jbi4cicsConfigurationMBean{

  private boolean fakeBooleanProperty;

  public Jbi4cicsComponentConfiguration() {
  }

  public boolean isFakeBooleanProperty() {
    return fakeBooleanProperty;
  }

  public void setFakeBooleanProperty(final boolean fakeBooleanProperty){
    this.fakeBooleanProperty = fakeBooleanProperty;
  }
}
