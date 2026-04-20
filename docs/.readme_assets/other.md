## Tips
### Vim Fold Markers
closed issues/sub-issues wrap their content in vim fold markers using `{{{always` suffix.
To auto-close these folds in nvim, add:
```lua
vim.opt.foldtext = [[substitute(getline(v:foldstart),'{{{]] .. [[always\s*$','{{{','')]] -- Custom foldtext that strips "always" from fold markers
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		vim.defer_fn(function()
			vim.cmd([[silent! g/{{]] .. [[{always$/normal! zc]])
		end, 10)
	end,
})
```
