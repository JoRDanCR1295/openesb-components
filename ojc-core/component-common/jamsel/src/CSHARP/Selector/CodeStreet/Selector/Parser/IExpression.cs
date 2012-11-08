/**
 * Copyright 2003, 2004, 2005. CodeStreet LLC.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
namespace CodeStreet.Selector.Parser
{
	
	/// <summary> Interface implemented by expression implementations.</summary>
	/// <author>  Jawaid Hakim.
	/// </author>
	public interface IExpression
		{
			/// <summary> Evaluate the expression.</summary>
			/// <param name="identifiers">Identifier values.
			/// </param>
			/// <returns> Result of the expression evaluation.
			/// </returns>
			System.Object eval(System.Collections.IDictionary identifiers);
			
			/// <summary> Evaluate the expression.</summary>
			/// <param name="provider">Value provider. During evaluation of the expression callbacks
			/// are made on the value provider to get identifier values.
			/// </param>
			/// <param name="corr">Correlation data. Passed as-is to the value provider.
			/// </param>
			/// <returns> Result evaluating the expression.
			/// </returns>
			System.Object eval(IValueProvider provider, System.Object corr);
		}
}