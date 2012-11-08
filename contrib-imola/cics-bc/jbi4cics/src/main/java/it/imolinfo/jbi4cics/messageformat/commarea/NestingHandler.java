/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.messageformat.commarea;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class NestingHandler {

  /**
   * The responsible to translate localized messages.
   */
   private static final Messages MESSAGES
          = Messages.getMessages(NestingHandler.class);

  /**
   * questo metodo prende in ingresso una commarea piatta come
   * ritornata dal parser e crea una definizione con i dovuti nesting
   * @param commareaBeanMappingDescriptor
   * @return
   * @throws FormatException
   */
  public static CommareaBeanMappingDescriptor handleNesting(CommareaBeanMappingDescriptor commareaBeanMappingDescriptor) throws FormatException{
    CommareaBeanMappingDescriptor result=new CommareaBeanMappingDescriptor();
    CommareaBeanMappingDescriptor currentCommareaBeanMappingDescriptor=result;
    Integer currentLevel=0;
    Stack<CommareaBeanMappingDescriptor> commareaStack=new Stack<CommareaBeanMappingDescriptor>();
    Stack<Integer> levelStack=new Stack<Integer>();
    Map<String,FieldDescriptor> fieldMap=commareaBeanMappingDescriptor.getFieldMap();
    List<String> fieldList=new ArrayList<String>(fieldMap.keySet());
    for (int i=0;i<fieldList.size();i++){
      //coversione sicura
      String propertyName=fieldList.get(i);
      CobolTypeDescriptor cobolTypeDescriptor=(CobolTypeDescriptor)fieldMap.get(propertyName);

      if (i==0) {
        //trattamento speciale per il primo perche' setta il livello di riferimento
        currentLevel=cobolTypeDescriptor.getLevel();
      }
      // se siamo alla fine si aggiunge il campo attuale nel mapping corrente e si prosegue
      if (i==(fieldList.size()-1)){
        currentCommareaBeanMappingDescriptor.addFieldMapping(propertyName, cobolTypeDescriptor.getName(),cobolTypeDescriptor);
        continue;
      }

      Integer nextLevel=((CobolTypeDescriptor)fieldMap.get(fieldList.get(i+1))).getLevel();
      switch (currentLevel.compareTo(nextLevel)) {
        case 0 : {
          //se il prossimo ha lo stesso livello allora si copia il campo attuale nel mapping corrente e si prosegue
          currentCommareaBeanMappingDescriptor.addFieldMapping(propertyName, cobolTypeDescriptor.getName(),cobolTypeDescriptor);
          break;
        }
        case -1 :{
          //se il prossimo campo ha un livello maggiore allora inizia un nesting
          //controllo che il campo attuale sia di tipo nesting
          if (cobolTypeDescriptor.getType()!=CobolType.NESTED_COMMAREA && cobolTypeDescriptor.getType()!=CobolType.OCCURS){
            throw new FormatException(MESSAGES.
                    getString("CIC001704_Not_nested_field", new Object[] {
                        cobolTypeDescriptor.getName(), currentLevel, nextLevel}));
          }
          //si copia il campo attuale nel mapping corrente e si prosegue e si mette livello e commarea nello stack, si istanzia il nuovo livello e una nuova commare descriptor
          currentCommareaBeanMappingDescriptor.addFieldMapping(propertyName, cobolTypeDescriptor.getName(),cobolTypeDescriptor);
          levelStack.push(currentLevel);
          currentLevel=nextLevel;
          commareaStack.push(currentCommareaBeanMappingDescriptor);
          currentCommareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
          cobolTypeDescriptor.setNestedCommarea(currentCommareaBeanMappingDescriptor);
          break;
        }
        case 1 : {
          //se il prossimo campo ha un livello minore allora finisce un nestin
          //si copia il campo attuale nel mapping corrente, e si fa il pop dei precednti livelli e commarea descriptor
          currentCommareaBeanMappingDescriptor.addFieldMapping(propertyName, cobolTypeDescriptor.getName(),cobolTypeDescriptor);
          currentLevel=levelStack.pop();
          currentCommareaBeanMappingDescriptor=commareaStack.pop();
          break;
        }
        default : {
          throw new FormatException(MESSAGES.getString("CIC001705_Unreachable_code"));
        }
      }
    }

    return result;
  }
}
