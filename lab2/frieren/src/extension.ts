import * as vscode from 'vscode';
import { FrierenHoverProvider } from './hoverProvider';

export function activate(context: vscode.ExtensionContext) {
    console.log('Frieren language extension is now active!');
    const provider = new FrierenHoverProvider();
    context.subscriptions.push(vscode.languages.registerHoverProvider('frieren', provider));
}
