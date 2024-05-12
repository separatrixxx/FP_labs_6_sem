import * as vscode from 'vscode';

export class FrierenHoverProvider implements vscode.HoverProvider {
    public provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Hover> {
        const wordRange = document.getWordRangeAtPosition(position);
        if (wordRange) {
            const word = document.getText(wordRange);

            if (word === 'lt') {
                return new vscode.Hover(`**let**\n\nLink Trinket: Объявление переменной в виде *(lt NAME = VALUE)* или *(lt NAME : VALUE)*`);    
            }
            if (word === 'mut') {
                const markdownMut = new vscode.MarkdownString();
                markdownMut.appendMarkdown(`**mut**\n\nМутация переменной: введите новое значение`);
                return new vscode.Hover(markdownMut, wordRange);  
            }
            if (word === 'cast') {
                const markdownCast = new vscode.MarkdownString();
                markdownCast.appendMarkdown(`**cast**\n\nВывод в консоль`);
                return new vscode.Hover(markdownCast, wordRange);  
            }
            if (word === 'if' || word === 'else') {
                const markdownIf = new vscode.MarkdownString();
                markdownIf.appendMarkdown(`**if else**\n\nУсловные операторы: *(if (COND) => () else => ())*`);
                return new vscode.Hover(markdownIf, wordRange);    
            }
            if (word === 'for') {
                const markdownFor = new vscode.MarkdownString();
                markdownFor.appendMarkdown(`**for**\n\nЦикл for: *(for NAME [VALUE1 .. VALUE2] => ())*`);
                return new vscode.Hover(markdownFor, wordRange);   
            }
            if (word === 'while') {
                const markdownWhile = new vscode.MarkdownString();
                markdownWhile.appendMarkdown(`**while**\n\nЦикл while: *(while (COND) => ())*`);
                return new vscode.Hover(markdownWhile, wordRange);
            }
            if (word === 'heal') {
                const markdownHeal = new vscode.MarkdownString();
                markdownHeal.appendMarkdown(`**heal**\n\nПоддерживайте свой цикл`);
                return new vscode.Hover(markdownHeal, wordRange);  
            }
            if (word === 'kill') {
                const markdownKill = new vscode.MarkdownString();
                markdownKill.appendMarkdown(`**kill**\n\nУбейте свой цикл`);
                return new vscode.Hover(markdownKill, wordRange);
            }
            if (word === 'spell') {
                const markdownSpell = new vscode.MarkdownString();
                markdownSpell.appendMarkdown(`**spell**\n\nСоздайте сложное заклинание: *(spell NAME {ARGS} => ())*`);
                return new vscode.Hover(markdownSpell, wordRange);
            }
            if (word === 'readGrim') {
                const markdownRead = new vscode.MarkdownString();
                markdownRead.appendMarkdown(`**readGrim**\n\nВы нашли гримуар, прочитайте его: *(readGrim "/path/to/file")*`);
                return new vscode.Hover(markdownRead, wordRange);
            }
            if (word === 'writeGrim') {
                const markdownWrite = new vscode.MarkdownString();
                markdownWrite.appendMarkdown(`**writeGrim**\n\nВы можете написать свой гримуар: *(writeGrim "/path/to/file" "TEXT")*`);
                return new vscode.Hover(markdownWrite, wordRange);
            }
            if (word === 'true' || word === 'false') {
                const markdownBool = new vscode.MarkdownString();
                markdownBool.appendMarkdown(`**true false**\n\nЛогические значения`);
                return new vscode.Hover(markdownBool, wordRange);
            }
            if (word === 'flowerField') {
                const markdownFlower = new vscode.MarkdownString();
                markdownFlower.appendMarkdown(`**flowerField**\n\nСамая красивая магия: создайте поле цветов с помощью *(flowerField)*`);
                return new vscode.Hover(markdownFlower, wordRange);
            }
        }
    }
}
